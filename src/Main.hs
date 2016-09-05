{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Camera
import RaytracingBook.BVH
import RaytracingBook.Hitable
import RaytracingBook.Monad
import RaytracingBook.Ray
import RaytracingBook.Sphere

import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Proxy
import Options.Applicative
import Linear
import Linear.Affine
import System.IO
import Unsafe.Coerce
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import qualified Vision.Image.JuicyPixels as FR
import qualified Vision.Image.Storage.DevIL as FR
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified System.Random.MWC as MWC

color :: (Floating f, Hitable f h, Epsilon f, Real f) => Ray f -> h -> Word -> Rayer (V3 Float)
color ray world depth =
    case hit world ray (0.0001) 10000 of
      Just rec
          | depth < 50 -> do
              mScattered <- scatter (rec ^.hit_material) ray rec
              case mScattered of
                Just (attenuation, scattered) ->
                    (attenuation *) <$> color scattered world (depth+1)
                Nothing -> pure 0
          | otherwise -> pure 0
      Nothing -> pure $
          let unit_direction = normalize $ fmap realToFrac $ ray^.ray_direction
              t = 0.5 * unit_direction^._y+1
          in (1-t)*^V3 1 1 1 + t*^V3 0.5 0.7 1.0

randomCoordinate :: (MWC.Variate f, Num f) => f -> Rayer (Point V3 f)
randomCoordinate radius =
    P <$> (V3 <$> uniformR (-10,10) <*> pure radius <*> uniformR (-10,10))

randomColor :: MWC.Variate f => Rayer (V3 f)
randomColor = V3 <$> uniform <*> uniform <*> uniform

randomMaterial :: (MWC.Variate f, Floating f, Ord f, Epsilon f) => Rayer (Material f)
randomMaterial = do
    rnd <- uniform :: Rayer Float
    if | rnd < 0.4 -> do
           col <- randomColor
           pure $ lambertian col
       | rnd < 0.8 -> do
           col <- randomColor
           fuzz <- uniform
           pure $ metal col fuzz
       | otherwise -> do
           nt <- uniformR (1.3,2)
           pure $ dielectric nt

randomSphere :: (MWC.Variate f, Floating f, Ord f, Epsilon f) => Rayer (Sphere f)
randomSphere = do
    rad <- uniformR (0.2,0.6)
    coord <- randomCoordinate rad
    mat <- randomMaterial
    pure Sphere
         { _sphere_center = coord
         , _sphere_radius = rad
         , _sphere_material = mat
         }

randomWorld :: (Floating f, Ord f, MWC.Variate f, Epsilon f) => Int -> Rayer (BoundingBox f)
randomWorld n = do
    let ground =
            Sphere
            { _sphere_center = P (V3 0 (-10000) (-1))
            , _sphere_radius = 10000
            , _sphere_material = lambertian (V3 0.8 0.8 0.0)
            }
    marbles <- V.replicateM n randomSphere
    pure $! initializeBVH $ V.map getBoundedHitableItem $ ground `V.cons` marbles

computeImage :: forall f. (Floating f, Ord f, MWC.Variate f, Epsilon f, VSM.Storable f, Real f) => Proxy f -> Int -> Int -> Int -> Task (JP.Image JP.PixelRGBF)
computeImage _ nx ny ns = do
    let camOpts =
            defaultCameraOpts
            & lookfrom .~ P (V3 10 4 10)
            & lookat .~ P (V3 2 0 2)
            & focusDist .~ sqrt (quadrance (V3 10 4 10 - V3 2 0 2))
            & aperture .~ 0.03
            & aspect .~ fromIntegral nx / fromIntegral ny
            & hfov .~ 100
        cam = getCamera camOpts
    liftIO $ putStrLn "Generating World"
    world <- liftIO $ runRayer (randomWorld 100) :: Task (BoundingBox f)
    let chunk_length = max 64 ((262144+ns-1) `div` ns)
        chunks = (nx*ny+chunk_length-1) `div` chunk_length
        chunk_upper n
            | n<chunks-1 = (n+1)*chunk_length-1
            | otherwise = nx*ny
    liftIO $ putStr "Computing Samples "
    buffer <- liftIO $ (VSM.new (nx*ny) :: IO (VSM.IOVector (V3 Float)))
    let inv_nx = 1/fromIntegral nx
        inv_ny = 1/fromIntegral ny
    sample_offsets <- liftIO . runRayer $
        VS.replicateM ns $ V2 <$> normal 0 (inv_nx/2) <*> normal 0 (inv_nx/2)
    for_ [0..chunks-1] $ \chunk ->
        liftIO . runRayer $ do
            for_ [chunk*chunk_length..chunk_upper chunk] $ \n -> do
                let (j,i) = n `quotRem` nx
                    u = fromIntegral i * inv_nx
                    v = fromIntegral (ny - j) * inv_ny
                samples <- VS.forM sample_offsets $ \(V2 uD vD) -> do
                    let u' = u + uD
                        v' = v + vD
                    ray <- getRay cam u' v'
                    color ray world 0
                liftIO $ VSM.unsafeWrite buffer n $ (/fromIntegral ns) $ VS.sum samples
            liftIO $ putChar '.'
    pixelData <- liftIO $ VS.unsafeFreeze buffer
    let pixelData' = unsafeCoerce pixelData -- Trust me
    liftIO $ putStrLn "Done"
    return $! JP.Image nx ny pixelData'

data RenderingOpts =
    RenderingOpts
    { opts_width :: Int
    , opts_height :: Int
    , opts_samples :: Int
    , opts_output :: FilePath
    , opts_useFloat :: Bool
    }

parseOpts :: Parser RenderingOpts
parseOpts =
    RenderingOpts
    <$> option auto
        ( long "width"
       <> metavar "WIDTH"
       <> help "Width in pixels"
       <> value 800 )
    <*> option auto
        ( long "height"
       <> metavar "HEIGHT"
       <> help "Height in pixels"
       <> value 600 )
    <*> option auto
        ( long "samples"
       <> metavar "SAMPLES"
       <> help "Number of samples per pixel"
       <> value 10 )
    <*> strOption
        ( long "output"
       <> metavar "FILEPATH"
       <> help "Filename to write the result to" )
    <*> switch
        ( long "use-float"
       <> help "Use Float for Rays" )

main :: IO ()
main = do
    let optsP = info (helper <*> parseOpts)
            ( fullDesc )
    opts <- execParser optsP
    let nx = opts_width opts
        ny = opts_height opts
        samples = opts_samples opts
    hSetBuffering stdout NoBuffering
    nt <- getNumCapabilities
    image <- withTaskGroup nt $ \tg -> runTask tg $
        if opts_useFloat opts
           then computeImage (Proxy :: Proxy Float) nx ny samples
           else computeImage (Proxy :: Proxy Double) nx ny samples
    let image_corrected = JP.gammaCorrection 2 image
        image_8 = JP.convertRGB8 $ JP.ImageRGBF image_corrected
        imageFriday = FR.toFridayRGB image_8
        out = opts_output opts
    res <- liftIO $ FR.save FR.Autodetect out $ imageFriday
    case res of
      Just err -> print err
      Nothing -> putStrLn $ "Saved as " ++ out
