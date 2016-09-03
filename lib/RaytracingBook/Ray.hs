{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Ray where

import Linear.V3
import Linear.Affine
import Linear.Vector
import Control.Lens

data Ray =
    Ray
    { _ray_origin :: !(Point V3 Float)
    , _ray_direction :: !(V3 Float)
    }
makeLenses ''Ray

{-# INLINE pointAtParameter #-}
pointAtParameter :: Ray -> Float -> Point V3 Float
pointAtParameter ray t = ray^.ray_origin .+^ t *^ ray^.ray_direction
