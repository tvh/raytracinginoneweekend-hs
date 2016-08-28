{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Sphere where

import RaytracingBook.Hitable
import RaytracingBook.Ray

import Linear
import Control.Lens
import Control.Monad

data Sphere =
    Sphere
    { _sphere_center :: !(V3 Float)
    , _sphere_radius :: !Float
    , _sphere_material :: Material
    }
makeLenses ''Sphere

instance Hitable Sphere where
    hit sphere ray t_min t_max =
        do let oc = ray^.origin - sphere^.sphere_center
               a = dot (ray^.direction) (ray^.direction)
               b = dot oc (ray^.direction)
               c = dot oc oc - sphere^.sphere_radius * sphere^.sphere_radius
               discriminant = b*b - a*c
           guard (discriminant > 0)
           let temp1 = ((-b) - sqrt (b*b-a*c)) / a
               temp2 = ((-b) + sqrt (b*b-a*c)) / a
           rec_t <-
               if | temp1 < t_max && temp1 > t_min -> Just temp1
                  | temp2 < t_max && temp2 > t_min -> Just temp2
                  | otherwise -> Nothing
           let rec_p = pointAtParameter ray rec_t
               rec_normal = (rec_p - sphere^.sphere_center) ^/ sphere^.sphere_radius
           Just HitRecord
                { _t = rec_t
                , _p = rec_p
                , _normal = rec_normal
                , _material = sphere^.sphere_material
                }
