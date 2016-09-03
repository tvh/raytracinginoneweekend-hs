{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Camera
import RaytracingBook.Hitable
import RaytracingBook.Monad
import RaytracingBook.Ray
import RaytracingBook.Sphere

import Control.Concurrent
import Control.Concurrent.Async.Pool
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Linear
import Linear.Affine
import System.Environment
import System.IO
import qualified Vision.Primitive as FR
import qualified Vision.Image as FR
import qualified Vision.Image.Storage.DevIL as FR
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

color :: Ray -> HitableList -> Word -> Rayer (V3 Float)
color ray world depth =
    case hit world ray (0.0001) 10000 of
      Just rec
          | depth < 50 -> do
              mScattered <- scatter (rec ^.material) ray rec
              case mScattered of
                Just (attenuation, scattered) ->
                    (attenuation *) <$> color scattered world (depth+1)
                Nothing -> pure 0
          | otherwise -> pure 0
      Nothing -> pure $
          let unit_direction = normalize $ ray^.ray_direction
              t = 0.5 * unit_direction^._y+1
          in (1-t)*^V3 1 1 1 + t*^V3 0.5 0.7 1.0

randomCoordinate :: Float -> Rayer (Point V3 Float)
randomCoordinate radius =
    P <$> (V3 <$> uniformR (-10,10) <*> pure radius <*> uniformR (-10,10))

randomColor :: Rayer (V3 Float)
randomColor = V3 <$> uniform <*> uniform <*> uniform

randomMaterial :: Rayer Material
randomMaterial = do
    rnd <- uniform
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

randomSphere :: Rayer Sphere
randomSphere = do
    rad <- uniformR (0.2,0.6)
    coord <- randomCoordinate rad
    mat <- randomMaterial
    pure Sphere
         { _sphere_center = coord
         , _sphere_radius = rad
         , _sphere_material = mat
         }

randomWorld :: Int -> Rayer HitableList
randomWorld n = do
    let ground =
            Sphere
            { _sphere_center = P (V3 0 (-500) (-1))
            , _sphere_radius = 500
            , _sphere_material = lambertian (V3 0.8 0.8 0.0)
            }
    marbles <- V.replicateM n randomSphere
    pure $ HitableList $ V.map toHitableItem $ ground `V.cons` marbles

computeImage :: Int -> Int -> Task FR.RGB
computeImage nx ny = do
    let camOpts =
            defaultCameraOpts
            & lookfrom .~ P (V3 3 3 2)
            & lookat .~ P (V3 0 0 (-1))
            & focusDist .~ sqrt (quadrance (V3 3 3 2 - V3 0 0 (-1)))
            & aperture .~ 0.1
            & aspect .~ fromIntegral nx / fromIntegral ny
            & hfov .~ 120
        cam = getCamera camOpts
    liftIO $ putStrLn "Generating World"
    world <- liftIO $ runRayer (randomWorld 100)
    let chunk_length = 262144
        chunks = (nx*ny+chunk_length-1) `div` chunk_length
        chunk_upper n
            | n<chunks-1 = (n+1)*chunk_length-1
            | otherwise = nx*ny
    liftIO $ putStr "Computing Samples "
    buffer <- liftIO $ VSM.new (nx*ny)
    for_ [0..chunks-1] $ \chunk ->
        liftIO . runRayer $ do
            for_ [chunk*chunk_length..chunk_upper chunk] $ \n -> do
                let (j,i) = n `quotRem` nx
                let u = fromIntegral i / fromIntegral nx
                    v = fromIntegral (ny - j) / fromIntegral ny
                ray <- getRay cam u v
                V3 r g b <- fmap round . (* 255) <$> color ray world 0
                liftIO $ VSM.unsafeWrite buffer n (FR.RGBPixel r g b)
            liftIO $ putChar '.'
    pixelData <- liftIO $ VS.unsafeFreeze buffer
    return $! FR.Manifest (FR.ix2 ny nx) pixelData

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    nt <- getNumCapabilities
    let nx = 1920 :: Int
        ny = 1080 :: Int
        samples_root = 4 :: Int
    image <- withTaskGroup nt $ \tg -> runTask tg $ computeImage (nx*samples_root) (ny*samples_root)
    [out] <- liftIO getArgs
    res <- liftIO $ FR.save FR.Autodetect out $ image
    case res of
      Just err -> print err
      Nothing -> putStrLn $ "Saved as " ++ out
