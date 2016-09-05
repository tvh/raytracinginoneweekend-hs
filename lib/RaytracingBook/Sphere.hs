{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Sphere where

import RaytracingBook.Hitable
import RaytracingBook.BVH
import RaytracingBook.Ray

import Linear
import Linear.Affine
import Control.Lens
import Control.Monad

data Sphere f =
    Sphere
    { _sphere_center :: !(Point V3 f)
    , _sphere_radius :: !f
    , _sphere_material :: !(Material f)
    }
makeLenses ''Sphere

instance (Floating f, Ord f) => Hitable f (Sphere f) where
    hit sphere ray t_min t_max =
        do let oc = ray^.ray_origin .-. sphere^.sphere_center
               a = quadrance (ray^.ray_direction)
               b = dot oc (ray^.ray_direction)
               c = quadrance oc - (sphere^.sphere_radius)*(sphere^.sphere_radius)
               discriminant = b*b - a*c
           guard (discriminant > 0)
           let temp1 = ((-b) - sqrt (b*b-a*c)) / a
               temp2 = ((-b) + sqrt (b*b-a*c)) / a
           rec_t <-
               if | temp1 < t_max && temp1 > t_min -> Just temp1
                  | temp2 < t_max && temp2 > t_min -> Just temp2
                  | otherwise -> Nothing
           let rec_p = pointAtParameter ray rec_t
               rec_normal = (rec_p .-. sphere^.sphere_center) ^/ sphere^.sphere_radius
           Just HitRecord
                { _hit_t = rec_t
                , _hit_p = rec_p
                , _hit_normal = rec_normal
                , _hit_material = sphere^.sphere_material
                }

instance (Floating f, Ord f) => BoundedHitable f (Sphere f) where
    {-# INLINE boundingBox #-}
    boundingBox sphere =
        ( sphere^.sphere_center .-^ pure (sphere^.sphere_radius)
        , sphere^.sphere_center .+^ pure (sphere^.sphere_radius)
        )
    {-# INLINE centroid #-}
    centroid sphere = sphere^.sphere_center
