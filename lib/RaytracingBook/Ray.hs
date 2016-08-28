{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Ray where

import Linear.V3
import Linear.Vector
import Control.Lens

data Ray =
    Ray
    { _origin :: !(V3 Float)
    , _direction :: !(V3 Float)
    }
makeLenses ''Ray

{-# INLINE pointAtParameter #-}
pointAtParameter :: Ray -> Float -> V3 Float
pointAtParameter ray t = ray^.origin + t *^ ray^.direction
