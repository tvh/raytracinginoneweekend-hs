{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Camera
import RaytracingBook.Hitable
import RaytracingBook.Monad
import RaytracingBook.Ray
import RaytracingBook.Sphere

import Codec.Picture
import Control.Concurrent.Async
import Control.Lens
import Linear
import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

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
          let unit_direction = normalize $ ray^.direction
              t = 0.5 * unit_direction^._y+1
          in (1-t)*^V3 1 1 1 + t*^V3 0.5 0.7 1.0

randomCoordinate :: Float -> Rayer (V3 Float)
randomCoordinate radius =
    V3 <$> uniformR (-20,20) <*> pure radius <*> uniformR (-20,20)

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
            { _sphere_center = V3 0 (-500) (-1)
            , _sphere_radius = 500
            , _sphere_material = lambertian (V3 0.8 0.8 0.0)
            }
    marbles <- V.replicateM n randomSphere
    pure $ HitableList $ V.map (HitableItem . hit) $ ground `V.cons` marbles

main :: IO ()
main = do
    [out] <- getArgs
    let nx = 1920 :: Int
        ny = 1080 :: Int
        ns = 16 :: Int
        nt = 8 :: Int
        camOpts =
            defaultCameraOpts
            & lookfrom .~ V3 3 3 2
            & lookat .~ V3 0 0 (-1)
            & focusDist .~ sqrt (quadrance (V3 3 3 2 - V3 0 0 (-1)))
            & aperture .~ 0.1
            & aspect .~ fromIntegral nx / fromIntegral ny
            & hfov .~ 120
        cam = getCamera camOpts
    world <- runRayer (randomWorld 300)
    imgDatas <-
        forConcurrently (V.fromList [1..nt]) $ \_ ->
        VS.generateM (nx*ny) $ \n ->
        runRayer $ do
        let (j,i) = n `quotRem` nx
        cols <- VS.replicateM ns $ do
            uD <- drand48
            vD <- drand48
            let u = (fromIntegral i + uD) / fromIntegral nx
                v = (fromIntegral (ny - j) + vD) / fromIntegral ny
            r <- getRay cam u v
            color r world 0
        pure $ VS.sum cols / fromIntegral ns
    let gamma = 1.8
        imgData = VS.map ((** (1/gamma)) . (/ fromIntegral nt)) $ V.foldl1' (VS.zipWith (+)) imgDatas
        image = generateImage (\i j -> let V3 r g b = VS.unsafeIndex imgData (j*nx+i) in PixelRGBF r g b) nx ny
    savePngImage out $ ImageRGBF image
