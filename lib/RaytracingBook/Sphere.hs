{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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

instance (RealFloat f, Ord f) => Hitable f (Sphere f) where
    {-# SPECIALISE instance Hitable Float (Sphere Float) #-}
    {-# SPECIALISE instance Hitable Double (Sphere Double) #-}
    hit sphere ray t_min t_max =
        do let oc = sphere^.sphere_center .-. ray^.ray_origin
               a = quadrance (ray^.ray_direction)
               b = dot oc (ray^.ray_direction)
               c = quadrance oc - (sphere^.sphere_radius)*(sphere^.sphere_radius)
               discriminant = b*b - a*c
           guard (discriminant > 0)
           let temp1 = (b - sqrt discriminant) / a
               temp2 = (b + sqrt discriminant) / a
           rec_t <-
               if | temp1 < t_max && temp1 > t_min -> Just temp1
                  | temp2 < t_max && temp2 > t_min -> Just temp2
                  | otherwise -> Nothing
           let rec_p = pointAtParameter ray rec_t
               rec_normal = (rec_p .-. sphere^.sphere_center) ^/ sphere^.sphere_radius
               rec_p_normalized = (rec_p - sphere^.sphere_center) ^/ sphere^.sphere_radius
               phi = atan2 (rec_p_normalized^._z) (rec_p_normalized^._x)
               theta = asin (rec_p_normalized^._y)
               u = 1-(phi+pi) / (2*pi)
               v = (theta + pi/2) / pi
           Just HitRecord
                { _hit_t = rec_t
                , _hit_p = rec_p
                , _hit_uv = V2 u v
                , _hit_normal = rec_normal
                , _hit_material = sphere^.sphere_material
                }

instance (RealFloat f, Ord f) => BoundedHitable f (Sphere f) where
    {-# INLINE boundingBox #-}
    boundingBox sphere =
        ( sphere^.sphere_center .-^ pure (sphere^.sphere_radius)
        , sphere^.sphere_center .+^ pure (sphere^.sphere_radius)
        )
    {-# INLINE centroid #-}
    centroid sphere = sphere^.sphere_center
