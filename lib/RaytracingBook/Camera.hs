{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module RaytracingBook.Camera where
import RaytracingBook.Ray

import Linear
import Control.Lens

data Camera f =
    Camera
    { _lowerLeftCorner :: !(V3 f)
    , _horizontal :: !(V3 f)
    , _vertical :: !(V3 f)
    , _cameraOrigin :: !(V3 f)
    }
makeLenses ''Camera

defaultCamera :: Fractional f => Camera f
defaultCamera =
    Camera
    { _lowerLeftCorner = V3 (-2) (-1) (-1)
    , _horizontal = V3 4 0 0
    , _vertical = V3 0 2 0
    , _cameraOrigin = V3 0 0 0
    }

getRay :: Floating f => Camera f -> f -> f -> Ray f
getRay cam u v =
    Ray
    { _origin = cam^.cameraOrigin
    , _direction =
         cam^.lowerLeftCorner
         + (u *^ cam^.horizontal)
         + (v *^ cam^.vertical)
         - cam^.cameraOrigin
    }