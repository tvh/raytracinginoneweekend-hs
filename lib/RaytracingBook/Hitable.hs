{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RaytracingBook.Hitable where

import RaytracingBook.Ray
import RaytracingBook.Monad

import Linear
import Control.Lens
import Control.Monad
import qualified Data.Vector as V

newtype Material =
    Material { scatter :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray)) }

data HitRecord =
    HitRecord
    { _t :: !Float
    , _p :: !(V3 Float)
    , _normal :: !(V3 Float)
    , _material :: !Material
    }
makeLenses ''HitRecord

class Hitable a where
    hit :: a
        -> Ray
        -> Float
        -- ^ t_min
        -> Float
        -- ^ t_max
        -> Maybe HitRecord

newtype HitableItem =
    HitableItem { unHitableItem :: Ray -> Float -> Float -> Maybe HitRecord }

instance Hitable HitableItem where
    hit = unHitableItem

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
           let target = rec^.p + rec^.normal + rnd
               scattered = Ray (rec^.p) (target - rec^.p)
               attenuation = albedo
           pure (Just (attenuation, scattered))

reflect :: V3 Float -> V3 Float -> V3 Float
reflect v n = v - 2*dot v n*^n

metal :: V3 Float -> Material
metal albedo = Material scatterFun
  where
    scatterFun :: Ray -> HitRecord -> Rayer (Maybe (V3 Float, Ray))
    scatterFun r_in rec =
        do let reflected = reflect (normalize (r_in^.direction)) (rec^.normal)
               scattered = Ray (rec^.p) reflected
               attenuation = albedo
               res = if dot (scattered^.direction) (rec^.normal) > 0
                     then Just (attenuation, scattered)
                     else Nothing
           pure res
