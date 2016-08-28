{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RaytracingBook.Monad where

import Linear
import Control.Monad.Trans.Reader
import Control.Monad.Primitive
import System.Random.MWC

newtype Rayer a =
    Rayer { unRayer :: ReaderT GenIO IO a }
    deriving (Functor, Applicative, Monad)

instance PrimMonad Rayer where
    type PrimState Rayer = RealWorld
    primitive = Rayer . primitive

runRayer :: Rayer a -> IO a
runRayer (Rayer m) =
    withSystemRandom $ \gen -> runReaderT m gen

{-# INLINE drand48 #-}
drand48 :: Rayer Float
drand48 =
    do gen <- Rayer ask
       res <- uniform gen
       pure (res - 2**(-33))

{-# INLINE randomInUnitSphere #-}
randomInUnitSphere :: Rayer (V3 Float)
randomInUnitSphere = do
    rnd <- V3 <$> drand48 <*> drand48 <*> drand48
    let res = 2*rnd - 1
    if dot res res >= 1
      then randomInUnitSphere
      else pure res
