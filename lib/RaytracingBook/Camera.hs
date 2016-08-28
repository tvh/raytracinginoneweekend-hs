{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RaytracingBook.Camera where
import RaytracingBook.Ray

import Linear
import Control.Lens

data Camera =
    Camera
    { _lowerLeftCorner :: !(V3 Float)
    , _horizontal :: !(V3 Float)
    , _vertical :: !(V3 Float)
    , _cameraOrigin :: !(V3 Float)
    }
makeLenses ''Camera

camera :: Float -> Float -> Camera
camera hfov aspect =
    let theta = hfov*pi/180
        half_width = tan (theta/2)
        half_height = half_width/aspect
    in Camera
       { _lowerLeftCorner = V3 (-half_width) (-half_height) (-1)
       , _horizontal = V3 (2*half_width) 0 0
       , _vertical = V3 0 (2*half_height) 0
       , _cameraOrigin = V3 0 0 0
       }

getRay :: Camera -> Float -> Float -> Ray
getRay cam u v =
    Ray
    { _origin = cam^.cameraOrigin
    , _direction =
         cam^.lowerLeftCorner
         + (u *^ cam^.horizontal)
         + (v *^ cam^.vertical)
         - cam^.cameraOrigin
    }
