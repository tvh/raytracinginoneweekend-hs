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

defaultCamera :: Camera
defaultCamera =
    Camera
    { _lowerLeftCorner = V3 (-2) (-1) (-1)
    , _horizontal = V3 4 0 0
    , _vertical = V3 0 2 0
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
