{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Camera where

import RaytracingBook.Monad
import RaytracingBook.Ray

import Linear
import Linear.Affine
import Control.Lens
import qualified System.Random.MWC as MWC

newtype Camera f =
    Camera { getRay :: f -> f -> Rayer (Ray f) }

data CameraOpts f =
    CameraOpts
    { _lookfrom :: !(Point V3 f)
    , _lookat :: !(Point V3 f)
    , _vup :: !(V3 f)
    , _hfov :: !f
    , _aspect :: !f
    , _aperture :: !f
    , _focusDist :: !f
    }
makeLenses ''CameraOpts

defaultCameraOpts :: Fractional f => CameraOpts f
defaultCameraOpts =
    CameraOpts
    { _lookfrom = P (V3 0 0 0)
    , _lookat = P (V3 0 0 (-1))
    , _vup = V3 0 1 0
    , _hfov = 120
    , _aspect = 4/3
    , _aperture = 0.2
    , _focusDist = 1
    }

getCamera :: (Epsilon f, Floating f, Ord f, MWC.Variate f) => CameraOpts f -> Camera f
getCamera opts =
    let lens_radius = opts^.aperture / 2
        theta = opts^.hfov*pi/180
        half_width = tan (theta/2)
        half_height = half_width/opts^.aspect
        w = normalize (opts^.lookfrom .-. opts^.lookat)
        u = normalize (cross (opts^.vup) w)
        v = cross w u
        lower_left_corner = opts^.lookfrom .-^ half_width*opts^.focusDist*^u .-^ half_height*opts^.focusDist*^v .-^ opts^.focusDist*^w
        horizontal = 2*half_width*opts^.focusDist *^ u
        vertical = 2*half_height*opts^.focusDist *^ v
        camera_origin = opts^.lookfrom
        getRayFun s t = do
            rd <- (lens_radius *^) <$> randomInUnitDisk
            let offset = u ^* rd^._x + v ^* rd^._y
            pure Ray
                 { _ray_origin = camera_origin .+^ offset
                 , _ray_direction =
                      lower_left_corner
                      .+^ (s *^ horizontal)
                      .+^ (t *^ vertical)
                      .-. camera_origin
                      - offset
                 }
    in Camera
       { getRay = getRayFun
       }
