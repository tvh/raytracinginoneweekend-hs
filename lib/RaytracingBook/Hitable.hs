{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RaytracingBook.Hitable where

import RaytracingBook.Ray
import RaytracingBook.Monad
import RaytracingBook.Texture

import Linear
import Linear.Affine
import Control.Lens
import Control.Monad
import qualified System.Random.MWC as MWC
import qualified Data.Vector as V

newtype Material f =
    Material { scatter :: Ray f -> HitRecord f -> Rayer (V3 Float, Maybe (V3 Float, Ray f)) }

data HitRecord f =
    HitRecord
    { _hit_t :: !f
    , _hit_p :: !(Point V3 f)
    , _hit_uv :: !(V2 f)
    , _hit_normal :: !(V3 f)
    , _hit_material :: !(Material f)
    }
makeLenses ''HitRecord

class Hitable f a | a -> f where
    -- | Check for a hit
    hit :: a
        -> Ray f
        -> f
        -- ^ t_min
        -> f
        -- ^ t_max
        -> Maybe (HitRecord f)

data HitableItem f =
    HitableItem
    { hi_hitFun :: !(Ray f -> f -> f -> Maybe (HitRecord f))
    }

instance Hitable f (HitableItem f) where
    hit = hi_hitFun
    {-# INLINE hit #-}

newtype HitableList f =
    HitableList { unHitableList :: V.Vector (HitableItem f) }

instance Hitable f (HitableList f) where
    hit (HitableList v) ray t_min t_max =
        V.foldl' merge Nothing v
      where
        merge :: Maybe (HitRecord f) -> (HitableItem f) -> Maybe (HitRecord f)
        merge mClosest (HitableItem hitFun) =
            let closest_t =
                    case mClosest of
                      Just closest -> closest^.hit_t
                      Nothing -> t_max
            in hitFun ray t_min closest_t `mplus` mClosest

normalColor :: (Floating f, Real f, Epsilon f) => Material f
normalColor = Material scatterFun
  where
    scatterFun _r_in rec = pure (fmap (realToFrac . (/2) . (+1)) (normalize $ rec^.hit_normal),Nothing)

lambertian :: forall f a. (MWC.Variate f, Fractional f, Ord f, Texture f a) => a -> Material f
lambertian albedo = Material scatterFun
  where
    scatterFun :: Ray f -> HitRecord f -> Rayer (V3 Float, Maybe (V3 Float, Ray f))
    scatterFun _r_in rec =
        do rnd <- randomInUnitSphere
           let target = rec^.hit_p + rec^.hit_normal.from _Point + rnd^.from _Point
               scattered = Ray (rec^.hit_p) (target .-. rec^.hit_p)
               attenuation = textureValue albedo (rec^.hit_uv) (rec^.hit_p)
           pure (0, Just (attenuation, scattered))

reflect :: Num f => V3 f -> V3 f -> V3 f
reflect v n = v - 2*dot v n*^n

metal :: forall f. (Fractional f, Ord f, MWC.Variate f, Floating f, Epsilon f) => V3 Float -> f -> Material f
metal albedo fuzz' = Material scatterFun
  where
    fuzz :: f
    fuzz = if fuzz' < 1 then fuzz' else 1
    scatterFun :: Ray f -> HitRecord f -> Rayer (V3 Float, Maybe (V3 Float, Ray f))
    scatterFun r_in rec =
        do rnd <- randomInUnitSphere
           let reflected = reflect (normalize (r_in^.ray_direction)) (rec^.hit_normal)
               scattered = Ray (rec^.hit_p) (reflected + fuzz*^rnd)
               attenuation = albedo
               res = if dot (scattered^.ray_direction) (rec^.hit_normal) > 0
                     then Just (attenuation, scattered)
                     else Nothing
           pure (0,res)

refract :: (Floating f, Ord f, Epsilon f) => V3 f -> V3 f -> f -> Maybe (V3 f)
refract v n ni_over_nt =
    let uv = normalize v
        dt = dot uv n
        discriminant = 1 - ni_over_nt*ni_over_nt*(1-dt*dt)
    in if (discriminant > 0)
       then Just (ni_over_nt*^(uv - n^*dt) - n^*sqrt discriminant)
       else Nothing

schlick :: Floating f => f -> f -> f
schlick cosine ref_idx =
    let r0' = (1-ref_idx) / (1+ref_idx)
        r0 = r0' * r0'
    in r0 + (1-r0)*(1-cosine)**5

dielectric :: forall f. (Epsilon f, MWC.Variate f, Floating f, Ord f) => f -> Material f
dielectric ref_idx = Material scatterFun
  where
    scatterFun :: Ray f -> HitRecord f -> Rayer (V3 Float, Maybe (V3 Float, Ray f))
    scatterFun r_in rec =
        do let attenuation :: V3 Float = 1
               cosine' = dot (r_in^.ray_direction) (rec^.hit_normal) / sqrt (quadrance (r_in^.ray_direction))
               (outward_normal, ni_over_nt, cosine) =
                   if dot (r_in^.ray_direction) (rec^.hit_normal) > 0
                      then (- rec^.hit_normal, ref_idx, ref_idx * cosine')
                      else (rec^.hit_normal, 1/ref_idx, -cosine')
               reflected = reflect (r_in^.ray_direction) (rec^.hit_normal)
           case refract (r_in^.ray_direction) outward_normal ni_over_nt of
             Just refracted ->
                 do let reflect_prob = schlick cosine ref_idx
                    rnd <- uniform
                    if rnd < reflect_prob
                      then pure $ (0, Just (attenuation, Ray (rec^.hit_p) reflected))
                      else pure $ (0, Just $ (attenuation, Ray (rec^.hit_p) refracted))
             Nothing -> pure $ (0, Just (attenuation, Ray (rec^.hit_p) reflected))
