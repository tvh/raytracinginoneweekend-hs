{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module RaytracingBook.Triangle where

import RaytracingBook.Hitable
import RaytracingBook.BVH
import RaytracingBook.Ray

import qualified Data.Vector as V
import Linear
import Linear.Affine
import Control.Lens
import Control.Monad
import Control.Monad.Zip

data Triangle f =
    Triangle
    { _triangle_vertices :: {-# UNPACK #-} !(V3 (Point V3 f))
    , _triangle_material :: !(Material f)
    }
makeLenses ''Triangle

instance (Floating f, Ord f, Epsilon f) => Hitable f (Triangle f) where
    {-# SPECIALISE instance Hitable Float (Triangle Float) #-}
    {-# SPECIALISE instance Hitable Double (Triangle Double) #-}
    hit triangle ray t_min t_max =
        do let V3 vert0 vert1 vert2 = triangle^.triangle_vertices
           -- find vectors for two edges sharing vert0
               edge1 = vert1 .-. vert0
               edge2 = vert2 .-. vert0
           -- begin calculating determinant  also used to calculate U parameter
               pvec = cross (ray^.ray_direction) edge2
           -- if determinant is near zero ray lies in plane of triangle
               det = dot edge1 pvec
           guard (not $ nearZero det)
           let inv_det = 1/det
           -- calculate distance from vert0 to ray origin
               tvec = ray^.ray_origin .-. vert0
           -- calculate U parameter and test bounds
               u = dot tvec pvec * inv_det
           guard (u>=0 && u<=1)
           -- prepare to test V parameter
           let qvec = cross tvec edge1
           -- calculate V parameter and test bounds
               v = dot (ray^.ray_direction) qvec * inv_det
           guard (v>=0 && u+v<=1)
           -- calculate t, ray intersects triangle
           let t = dot edge2 qvec * inv_det
           guard (t>t_min && t<t_max)
           let w = 1 - u - v
               p = u*^vert0 + v*^vert1 + w*^vert2
               n = normalize (cross edge1 edge2)
               uv = V2 0 1 ^* v + V2 1 1 ^* w
           Just HitRecord
                { _hit_t = t
                , _hit_p = p
                , _hit_uv = uv
                , _hit_normal = n
                , _hit_material = triangle^.triangle_material
                }

instance (Epsilon f, Floating f, Ord f) => BoundedHitable f (Triangle f) where
    {-# SPECIALISE instance BoundedHitable Float (Triangle Float) #-}
    {-# SPECIALISE instance BoundedHitable Double (Triangle Double) #-}
    {-# INLINABLE boundingBox #-}
    boundingBox triangle =
        let V3 (P a) (P b) (P c) = (triangle^.triangle_vertices)
            l1 = a
            h1 = a
            l2 = mzipWith min l1 b
            h2 = mzipWith max h1 b
            l3 = mzipWith min l2 c
            h3 = mzipWith max h2 c
        in (P l3, P h3)
    {-# INLINABLE centroid #-}
    centroid triangle = sum (triangle^.triangle_vertices) / 3
