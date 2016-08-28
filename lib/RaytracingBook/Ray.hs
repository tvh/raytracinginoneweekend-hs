{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Ray where

import Linear.V3
import Linear.Vector
import Control.Lens

data Ray a =
    Ray
    { _origin :: !(V3 a)
    , _direction :: !(V3 a)
    }
makeLenses ''Ray

{-# INLINE pointAtParameter #-}
pointAtParameter :: Num a => Ray a -> a -> V3 a
pointAtParameter ray t = ray^.origin + t *^ ray^.direction
