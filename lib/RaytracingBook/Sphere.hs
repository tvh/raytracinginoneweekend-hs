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

data Sphere f =
    Sphere
    { _center :: !(V3 f)
    , _radius :: !f
    }
makeLenses ''Sphere

instance (Num f, Ord f, Floating f) => Hitable f (Sphere f) where
    hit sphere ray t_min t_max =
        do let oc = ray^.origin - sphere^.center
               a = dot (ray^.direction) (ray^.direction)
               b = dot oc (ray^.direction)
               c = dot oc oc - sphere^.radius * sphere^.radius
               discriminant = b*b - a*c
           guard (discriminant > 0)
           let temp1 = ((-b) - sqrt (b*b-a*c)) / a
               temp2 = ((-b) + sqrt (b*b-a*c)) / a
           rec_t <-
               if | temp1 < t_max && temp1 > t_min -> Just temp1
                  | temp2 < t_max && temp2 > t_min -> Just temp2
                  | otherwise -> Nothing
           let rec_p = pointAtParameter ray rec_t
               rec_normal = (rec_p - sphere^.center) ^/ sphere^.radius
           Just HitRecord
                { _t = rec_t
                , _p = rec_p
                , _normal = rec_normal
                }
