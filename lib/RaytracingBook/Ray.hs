{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Ray where

import Linear.V3
import Linear.Affine
import Linear.Vector
import Control.Lens

data Ray f =
    Ray
    { _ray_origin :: !(Point V3 f)
    , _ray_direction :: !(V3 f)
    }
makeLenses ''Ray

{-# INLINE pointAtParameter #-}
pointAtParameter :: Num f => Ray f -> f -> Point V3 f
pointAtParameter ray t = ray^.ray_origin .+^ t *^ ray^.ray_direction
