{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RaytracingBook.Monad where

import Linear
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import System.IO.Unsafe

newtype Rayer a =
    Rayer { unRayer :: ReaderT MWC.GenIO IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

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
uniform :: MWC.Variate f => Rayer f
uniform = liftRandomFun MWC.uniform

{-# INLINE uniformR #-}
uniformR :: MWC.Variate f => (f, f) -> Rayer f
uniformR bounds = liftRandomFun (MWC.uniformR bounds)

{-# INLINE normal #-}
normal :: (Real f, Fractional f) => f -> f -> Rayer f
normal mean std =
    let mean' = realToFrac mean
        std' = realToFrac std
    in realToFrac <$> liftRandomFun (MWC.normal mean' std')

{-# INLINE standard #-}
standard :: Fractional f => Rayer f
standard = realToFrac <$> liftRandomFun MWC.standard

{-# INLINE randomInUnitSphere #-}
randomInUnitSphere :: (MWC.Variate f, Fractional f, Ord f) => Rayer (V3 f)
randomInUnitSphere = do
    res <- V3 <$> uniformR (-1,1) <*> uniformR (-1,1) <*> uniformR (-1,1)
    if quadrance res >= 1
      then randomInUnitSphere
      else pure res

{-# INLINE randomInUnitDisk #-}
randomInUnitDisk :: (MWC.Variate f, Fractional f, Ord f) => Rayer (V3 f)
randomInUnitDisk = do
    res <- V3 <$> uniformR (-1,1) <*> uniformR (-1,1) <*> pure 0
    if dot res res >= 1
      then randomInUnitSphere
      else pure res
