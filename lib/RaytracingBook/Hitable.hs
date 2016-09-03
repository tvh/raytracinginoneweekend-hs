{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RaytracingBook.Hitable where

import RaytracingBook.Ray
import RaytracingBook.Monad

import Linear
import Linear.Affine
import Control.Lens
import Control.Monad
import qualified Data.Vector as V

newtype Material =
    Material { scatter :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray)) }

data HitRecord =
    HitRecord
    { _t :: !Float
    , _p :: !(Point V3 Float)
    , _normal :: !(V3 Float)
    , _material :: !Material
    }
makeLenses ''HitRecord

class Hitable a where
    -- | Check for a hit
    hit :: a
        -> Ray
        -> Float
        -- ^ t_min
        -> Float
        -- ^ t_max
        -> Maybe HitRecord

data HitableItem =
    HitableItem
    { hi_hitFun :: !(Ray -> Float -> Float -> Maybe HitRecord)
    }

instance Hitable HitableItem where
    hit = hi_hitFun
    {-# INLINE hit #-}

newtype HitableList =
    HitableList { unHitableList :: V.Vector HitableItem }

instance Hitable HitableList where
    hit (HitableList v) ray t_min t_max =
        V.foldl' merge Nothing v
      where
        merge :: Maybe HitRecord -> HitableItem -> Maybe HitRecord
        merge mClosest (HitableItem hitFun) =
            let closest_t =
                    case mClosest of
                      Just closest -> closest^.t
                      Nothing -> t_max
            in hitFun ray t_min closest_t `mplus` mClosest

lambertian :: V3 Float -> Material
lambertian albedo = Material scatterFun
  where
    scatterFun :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray))
    scatterFun _r_in rec =
        do rnd <- randomInUnitSphere
           let target = rec^.p + rec^.normal.from _Point + rnd^.from _Point
               scattered = Ray (rec^.p) (target .-. rec^.p)
               attenuation = albedo
           pure (Just (attenuation, scattered))

reflect :: V3 Float -> V3 Float -> V3 Float
reflect v n = v - 2*dot v n*^n

metal :: V3 Float -> Float -> Material
metal albedo fuzz' = Material scatterFun
  where
    fuzz :: Float
    fuzz = if fuzz' < 1 then fuzz' else 1
    scatterFun :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray))
    scatterFun r_in rec =
        do rnd <- randomInUnitSphere
           let reflected = reflect (normalize (r_in^.ray_direction)) (rec^.normal)
               scattered = Ray (rec^.p) (reflected + fuzz*^rnd)
               attenuation = albedo
               res = if dot (scattered^.ray_direction) (rec^.normal) > 0
                     then Just (attenuation, scattered)
                     else Nothing
           pure res

refract :: V3 Float -> V3 Float -> Float -> Maybe (V3 Float)
refract v n ni_over_nt =
    let uv = normalize v
        dt = dot uv n
        discriminant = 1 - ni_over_nt*ni_over_nt*(1-dt*dt)
    in if (discriminant > 0)
       then Just (ni_over_nt*^(uv - n^*dt) - n^*sqrt discriminant)
       else Nothing

schlick :: Float -> Float -> Float
schlick cosine ref_idx =
    let r0' = (1-ref_idx) / (1+ref_idx)
        r0 = r0' * r0'
    in r0 + (1-r0)*(1-cosine)**5

dielectric :: Float -> Material
dielectric ref_idx = Material scatterFun
  where
    scatterFun :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray))
    scatterFun r_in rec =
        do let attenuation :: V3 Float = 1
               cosine' = dot (r_in^.ray_direction) (rec^.normal) / sqrt (quadrance (r_in^.ray_direction))
               (outward_normal, ni_over_nt, cosine) =
                   if dot (r_in^.ray_direction) (rec^.normal) > 0
                      then (- rec^.normal, ref_idx, ref_idx * cosine')
                      else (rec^.normal, 1/ref_idx, -cosine')
               reflected = reflect (r_in^.ray_direction) (rec^.normal)
           case refract (r_in^.ray_direction) outward_normal ni_over_nt of
             Just refracted ->
                 do let reflect_prob = schlick cosine ref_idx
                    rnd <- drand48
                    if rnd < reflect_prob
                      then pure $ Just (attenuation, Ray (rec^.p) reflected)
                      else pure $ Just $ (attenuation, Ray (rec^.p) refracted)
             Nothing -> pure $ Just (attenuation, Ray (rec^.p) reflected)
