{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Camera
import RaytracingBook.Hitable
import RaytracingBook.Ray
import RaytracingBook.Sphere

import Codec.Picture
import Control.Lens
import Linear
import System.Environment
import System.IO.Unsafe
import System.Random.MWC
import qualified Data.Vector as V

gen :: GenIO
gen = unsafePerformIO createSystemRandom
{-# NOINLINE gen #-}

drand48 :: IO Float
drand48 = (+ (-2**(-33))) <$> uniform gen

randomInUnitSphere :: IO (V3 Float)
randomInUnitSphere = do
    rnd <- V3 <$> drand48 <*> drand48 <*> drand48
    let res = 2*rnd - 1
    if dot res res >= 1
      then randomInUnitSphere
      else pure res

color :: Ray Float -> HitableList Float -> IO (V3 Float)
color ray world =
    case hit world ray (0.0001) 10000 of
      Just rec -> do
          rnd <- randomInUnitSphere
          let target = rec^.p + rec ^.normal + rnd
          (0.5 *) <$> color (Ray (rec^.p) (target - rec^.p)) world
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
        sphere1 = Sphere{ _center = V3 0 0 (-1), _radius = 0.5 }
        sphere2 = Sphere{ _center = V3 0 (-100.5) (-1), _radius = 100 }
        world = HitableList $ V.map (HitableItem . hit) [sphere1,sphere2]
        camera = defaultCamera
    image <- withImage nx ny $ \i j -> do
        cols <- V.forM [1..ns] $ \_ ->
            do uD <- drand48
               vD <- drand48
               let u = (fromIntegral i + uD) / fromIntegral nx
                   v = (fromIntegral (ny - j) + vD) / fromIntegral ny
                   r = getRay camera u v
               color r world
        let V3 ir ig ib = V.sum cols / fromIntegral ns
        pure $ PixelRGBF ir ig ib
    savePngImage out (ImageRGBF image)
