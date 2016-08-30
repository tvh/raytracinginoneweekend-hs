{-# LANGUAGE BangPatterns #-}
module Main where

import qualified System.Random.MWC as MWC
import qualified System.Random.Mersenne as MR
import qualified System.Random.Mersenne.Pure64 as MRP
import qualified System.Random as SR
import System.IO.Unsafe
import Control.Arrow
import Data.IORef
import Data.Word
import Data.Int
import Data.Bits
import qualified System.Random.Xorshift128Plus as XOS

import Criterion.Main

{-# NOINLINE genMWC #-}
genMWC :: MWC.GenIO
genMWC = unsafePerformIO MWC.create

{-# NOINLINE genMR #-}
genMR :: MR.MTGen
genMR = unsafePerformIO MR.getStdGen

{-# NOINLINE genStd #-}
genStd :: IORef SR.StdGen
genStd = unsafePerformIO $ newIORef =<< SR.newStdGen

getStdRandomNT :: (SR.StdGen -> (a,SR.StdGen)) -> IO a
getStdRandomNT f = do
    oldGen <- readIORef genStd
    let (x,!newGen) = f oldGen
    writeIORef genStd newGen
    return x

randomIONT :: SR.Random a => IO a
randomIONT = getStdRandomNT SR.random

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


main :: IO ()
main =
    defaultMain
    [ bench "System.Random.randomIO (threadsafe)" $ whnfIO (SR.randomIO :: IO Float)
    , bench "System.Random.randomIO (non-threadsafe)" $ whnfIO (randomIONT :: IO Float)
    , bench "System.Random.random" $ whnf (SR.random :: SR.StdGen -> (Float,SR.StdGen)) (SR.mkStdGen 123456)
    , bench "MWC.uniform" $ whnfIO (SR.randomIO :: IO Float)
    , bench "MR.randomIO (Word64)" $ whnfIO (MR.randomIO :: IO Word64)
    , bench "MR.randomIO (Double)" $ whnfIO (MR.randomIO :: IO Double)
    , bench "MR.random (Word64)" $ whnfIO (MR.random genMR :: IO Word64)
    , bench "MR.random (Double)" $ whnfIO (MR.random genMR :: IO Double)
    , bench "MRP.random (Word64)" $ whnf MRP.randomWord64 (MRP.pureMT 12345678)
    , bench "MRP.random (Double)" $ whnf MRP.randomDouble (MRP.pureMT 12345678)
    , bench "MRP.random (Float)" $ whnf (first (realToFrac :: Double -> Float) . MRP.randomDouble) (MRP.pureMT 12345678)
    , bench "xorShift32" $ whnf xorShift32 314159265
    , bench "xorShift32Float" $ whnf xorShift32Float 314159265
    , bench "xorShift64" $ whnf xorShift64 88172645463325252
    , bench "xorShift128+" $ whnf XOS.next (XOS.initialize 123456)
    ]