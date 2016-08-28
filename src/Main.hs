{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Camera
import RaytracingBook.Hitable
import RaytracingBook.Monad
import RaytracingBook.Ray
import RaytracingBook.Sphere

import Codec.Picture
import Control.Lens
import Linear
import System.Environment
import qualified Data.Vector as V

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

main :: IO ()
main = do
    [out] <- getArgs
    let nx = 400 :: Int
        ny = 200 :: Int
        ns = 100 :: Int
        spheres =
            [ Sphere
              { _sphere_center = V3 0 0 (-1)
              , _sphere_radius = 0.5
              , _sphere_material = lambertian (V3 0.8 0.3 0.3)
              }
            , Sphere
              { _sphere_center = V3 0 (-100.5) (-1)
              , _sphere_radius = 100
              , _sphere_material = lambertian (V3 0.8 0.8 0.0)
              }
            , Sphere
              { _sphere_center = V3 1 0 (-1)
              , _sphere_radius = 0.5
              , _sphere_material = metal (V3 0.8 0.6 0.2)
              }
            , Sphere
              { _sphere_center = V3 (-1) 0 (-1)
              , _sphere_radius = 0.5
              , _sphere_material = metal (V3 0.8 0.8 0.8)
              }
            ]
        world = HitableList $ V.map (HitableItem . hit) spheres
        camera = defaultCamera
    image <-
        withImage nx ny $ \i j ->
        runRayer $ do
        cols <- V.forM [1..ns] $ \_ ->
            do uD <- drand48
               vD <- drand48
               let u = (fromIntegral i + uD) / fromIntegral nx
                   v = (fromIntegral (ny - j) + vD) / fromIntegral ny
                   r = getRay camera u v
               color r world 0
        let V3 ir ig ib = V.sum cols / fromIntegral ns
        pure $ PixelRGBF ir ig ib
    savePngImage out (ImageRGBF image)
