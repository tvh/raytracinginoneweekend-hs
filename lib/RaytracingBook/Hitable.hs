{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RaytracingBook.Hitable where

import RaytracingBook.Ray

import Linear
import Control.Lens
import Control.Monad
import qualified Data.Vector as V

data HitRecord f =
    HitRecord
    { _t :: !f
    , _p :: !(V3 f)
    , _normal :: !(V3 f)
    }
makeLenses ''HitRecord

class Num f => Hitable f a | a -> f where
    hit :: a
        -> Ray f
        -> f
        -- ^ t_min
        -> f
        -- ^ t_max
        -> Maybe (HitRecord f)

newtype HitableItem f =
    HitableItem { unHitableItem :: Ray f -> f -> f -> Maybe (HitRecord f) }

instance Num f => Hitable f (HitableItem f) where
    hit = unHitableItem

newtype HitableList f =
    HitableList { unHitableList :: V.Vector (HitableItem f) }

instance Num f => Hitable f (HitableList f) where
    hit (HitableList v) ray t_min t_max =
        V.foldl' merge Nothing v
      where
        merge :: Maybe (HitRecord f) -> HitableItem f -> Maybe (HitRecord f)
        merge mClosest (HitableItem hitFun) =
            let closest_t =
                    case mClosest of
                      Just closest -> closest^.t
                      Nothing -> t_max
            in hitFun ray t_min closest_t `mplus` mClosest
