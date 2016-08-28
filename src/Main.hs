{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RaytracingBook.Hitable
import RaytracingBook.Sphere
import RaytracingBook.Ray
import RaytracingBook.Camera

import Linear
import Control.Lens
import qualified Data.Vector as V
import Control.Monad
import System.Random.MWC
import System.IO.Unsafe

gen :: GenIO
gen = unsafePerformIO createSystemRandom
{-# NOINLINE gen #-}

drand48 :: IO Float
drand48 = (+ (-2**(-33))) <$> uniform gen

randomInUnitSphere :: IO (V3 Float)
randomInUnitSphere = do
    rnd <- V3 <$> drand48 <*> drand48 <*> drand48
    let vector = 2*rnd - 1
    if dot vector vector >= 1
      then randomInUnitSphere
      else pure vector

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
    let nx = 200 :: Int
        ny = 100 :: Int
        ns = 100 :: Int
    putStrLn $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"
    let lower_left_corner :: V3 Float = V3 (-2) (-1) (-1)
        horizontal :: V3 Float = V3 4 0 0
        vertical :: V3 Float = V3 0 2 0
        origin :: V3 Float = V3 0 0 0
        sphere1 = Sphere{ _center = V3 0 0 (-1), _radius = 0.5 }
        sphere2 = Sphere{ _center = V3 0 (-100.5) (-1), _radius = 100 }
        world = HitableList $ V.map (HitableItem . hit) [sphere1,sphere2]
        camera = defaultCamera
    V.forM_ [ny-1,ny-2..0] $ \j ->
        V.forM_ [0..nx-1] $ \i -> do
            cols <- V.forM [1..ns] $ \_ ->
                do uD <- drand48
                   vD <- drand48
                   let u = (fromIntegral i + uD) / fromIntegral nx
                       v = (fromIntegral j + vD) / fromIntegral ny
                       r = getRay camera u v
                   color r world
            let col = sqrt $ V.sum cols / fromIntegral ns
            let ir :: Word = truncate $ (255.99 *) $ col ^. _x
                ig :: Word = truncate $ (255.99 *) $ col ^. _y
                ib :: Word = truncate $ (255.99 *) $ col ^. _y
            putStrLn $ show ir ++ " " ++ show ig ++ " " ++ show ib
