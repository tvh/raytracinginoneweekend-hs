{-# LANGUAGE BangPatterns #-}
module Main where

import RaytracingBook.BVH
import RaytracingBook.Ray
import RaytracingBook.Sphere
import RaytracingBook.Monad
import RaytracingBook.Hitable

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import qualified System.Random.Mersenne as MR
import qualified System.Random.Mersenne.Pure64 as MRP
import qualified Data.Vector as V
import Control.Monad
import System.IO.Unsafe
import Data.Word
import Data.Int
import Data.Bits
import Linear
import Linear.Affine
import qualified System.Random.Xorshift128Plus as XOS

import Criterion.Main

{-# NOINLINE genMWC #-}
genMWC :: MWC.GenIO
genMWC = unsafePerformIO MWC.create

{-# NOINLINE genMR #-}
genMR :: MR.MTGen
genMR = unsafePerformIO MR.getStdGen

xorShift32 :: Word32 -> Word32
xorShift32 !x1 =
    let !x2 = x1 `xor` shiftL x1 13
        !x3 = x2 `xor` shiftR x2 17
        !x4 = x3 `xor` shiftL x3 5
    in x4

wordToFloat :: Word32 -> Float
wordToFloat x = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

xorShift32Float :: Word32 -> (Float, Word32)
xorShift32Float !x1 =
  let !x2 = xorShift32 x1
      !f = wordToFloat x2
  in (f,x2)

xorShift64 :: Word64 -> Word64
xorShift64 !x1 =
    let !x2 = x1 `xor` shiftL x1 13
        !x3 = x2 `xor` shiftR x2 7
        !x4 = x3 `xor` shiftL x3 17
    in x4

getRandomSpheres :: (Epsilon f, Floating f, MWC.Variate f, Ord f) => Int -> IO (V.Vector (BoundedHitableItem f))
getRandomSpheres n =
    runRayer $ V.replicateM n $ do
        center <- P . (* (100 * (fromIntegral n ** (1/3)))) <$> randomInUnitSphere
        radius <- standard
        pure $ getBoundedHitableItem $
            Sphere
            { _sphere_center = center
            , _sphere_radius = radius
            , _sphere_material = dielectric 1
            }

prng_benchmarks :: [Benchmark]
prng_benchmarks =
    [ bench "MWC.uniform (Word32)" $ nfIO (MWC.uniform genMWC :: IO Word32)
    , bench "MWC.uniform (Word64)" $ nfIO (MWC.uniform genMWC :: IO Word64)
    , bench "MWC.uniform (Float)" $ nfIO (MWC.uniform genMWC :: IO Float)
    , bench "MWC.uniform (Double)" $ nfIO (MWC.uniform genMWC :: IO Double)
    , bench "MWC.standard (Double)" $ nfIO (MWC.standard genMWC :: IO Double)
    , bench "MWC.normal (Double)" $ nfIO (MWC.normal 2 2 genMWC :: IO Double)
    , bench "MR.randomIO (Word64)" $ nfIO (MR.randomIO :: IO Word64)
    , bench "MR.randomIO (Double)" $ nfIO (MR.randomIO :: IO Double)
    , bench "MR.random (Word64)" $ nfIO (MR.random genMR :: IO Word64)
    , bench "MR.random (Double)" $ nfIO (MR.random genMR :: IO Double)
    , bench "MRP.random (Word64)" $ nf (fst . MRP.randomWord64) (MRP.pureMT 12345678)
    , bench "MRP.random (Double)" $ nf (fst . MRP.randomDouble) (MRP.pureMT 12345678)
    , bench "MRP.random (Float)" $ whnf ((realToFrac :: Double -> Float) . fst . MRP.randomDouble) (MRP.pureMT 12345678)
    , bench "xorShift32" $ nf xorShift32 314159265
    , bench "xorShift32Float" $ nf xorShift32Float 314159265
    , bench "xorShift64" $ nf xorShift64 88172645463325252
    , bench "xorShift128+" $ nf (fst. XOS.next) (XOS.initialize 123456)
    ]

bvh_benchmarks :: [Benchmark]
bvh_benchmarks =
    [ env (getRandomSpheres 1000) $ \scene ->
        bench "initializeBVH (Float, 1000)" $
          whnf initializeBVH (scene :: V.Vector (BoundedHitableItem Float))
    , env (getRandomSpheres 1000) $ \scene ->
        bench "initializeBVH (Double, 1000)" $
          whnf initializeBVH (scene :: V.Vector (BoundedHitableItem Double))
    , env (getRandomSpheres 10000) $ \scene ->
        bench "initializeBVH (Float, 10000)" $
          whnf initializeBVH (scene :: V.Vector (BoundedHitableItem Float))
    , env (getRandomSpheres 10000) $ \scene ->
        bench "initializeBVH (Double, 10000)" $
          whnf initializeBVH (scene :: V.Vector (BoundedHitableItem Double))
    , env (getRandomSpheres 1000) $ \scene ->
        bench "hit BVH 1000" $
          whnf (\(item, ray) -> hit item ray 0 10000)
               ( initializeBVH scene :: BoundingBox Double
               , Ray (P (V3 1000 0 0)) (V3 (-1) 0 0 )
               )
    , env (getRandomSpheres 10000) $ \scene ->
        bench "hit BVH 10000" $
          whnf (\(item, ray) -> hit item ray 0 10000)
               ( initializeBVH scene :: BoundingBox Double
               , Ray (P (V3 1000 0 0)) (V3 (-1) 0 0 )
               )
    , env (getRandomSpheres 100000) $ \scene ->
        bench "hit BVH 100000" $
          whnf (\(item, ray) -> hit item ray 0 10000)
               ( initializeBVH scene :: BoundingBox Double
               , Ray (P (V3 1000 0 0)) (V3 (-1) 0 0 )
               )
    , env (getRandomSpheres 1000000) $ \scene ->
        bench "hit BVH 1000000 (Float)" $
          whnf (\(item, ray) -> hit item ray 0 10000)
               ( initializeBVH scene :: BoundingBox Float
               , Ray (P (V3 1000 0 0)) (V3 (-1) 0 0 )
               )
    , env (getRandomSpheres 1000000) $ \scene ->
        bench "hit BVH 1000000 (Double)" $
          whnf (\(item, ray) -> hit item ray 0 10000)
               ( initializeBVH scene :: BoundingBox Double
               , Ray (P (V3 1000 0 0)) (V3 (-1) 0 0 )
               )
    , bench "Sphere (hit)" $
        whnf (\(item, ray) -> hit item ray 0 1000)
             ( Sphere 0 1 (dielectric 1 :: Material Double)
             , Ray (P (V3 2 0 0)) (V3 (-1) 0 0 )
             )
    , bench "Sphere (miss)" $
        whnf (\(item, ray) -> hit item ray 0 1000)
             ( Sphere 0 1 (dielectric 1 :: Material Double)
             , Ray (P (V3 2 0 0)) (V3 0 1 0 )
             )
    ]

main :: IO ()
main =
    defaultMain
    [ bgroup "prng" prng_benchmarks
    , bgroup "bvh" bvh_benchmarks
    ]
