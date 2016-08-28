{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RaytracingBook.Monad where

import Linear
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool
import qualified System.Random.MWC as MWC
import System.IO.Unsafe

newtype Rayer a =
    Rayer { unRayer :: ReaderT MWC.GenIO IO a }
    deriving (Functor, Applicative, Monad)

{-# NOINLINE gens #-}
gens :: Pool MWC.GenIO
gens =
    unsafePerformIO $
    createPool
        MWC.createSystemRandom
        (const (return ()))
        32
        30
        128

runRayer :: Rayer a -> IO a
runRayer (Rayer m) =
    withResource gens $ \gen -> runReaderT m gen


{-# INLINE liftRandomFun #-}
liftRandomFun :: (MWC.GenIO -> IO a) -> Rayer a
liftRandomFun f = Rayer ask >>= Rayer . liftIO . f

{-# INLINE uniform #-}
uniform :: Rayer Float
uniform = liftRandomFun MWC.uniform

{-# INLINE uniformR #-}
uniformR :: (Float, Float) -> Rayer Float
uniformR bounds = liftRandomFun (MWC.uniformR bounds)

{-# INLINE drand48 #-}
drand48 :: Rayer Float
drand48 = do
    res <- uniform
    pure (res - 2**(-33))

{-# INLINE randomInUnitSphere #-}
randomInUnitSphere :: Rayer (V3 Float)
randomInUnitSphere = do
    res <- V3 <$> uniformR (-1,1) <*> uniformR (-1,1) <*> uniformR (-1,1)
    if quadrance res >= 1
      then randomInUnitSphere
      else pure res

{-# INLINE randomInUnitDisk #-}
randomInUnitDisk :: Rayer (V3 Float)
randomInUnitDisk = do
    res <- V3 <$> uniformR (-1,1) <*> uniformR (-1,1) <*> pure 0
    if dot res res >= 1
      then randomInUnitSphere
      else pure res
