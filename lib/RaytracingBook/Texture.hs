{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RaytracingBook.Texture where

import Control.Lens
import Linear
import Linear.Affine

class Texture f a | a -> f where
    textureValue :: a -> f -> f -> Point V3 f -> V3 Float

newtype ConstantTexture f =
    ConstantTexture (V3 Float)

instance Texture f (ConstantTexture f) where
    textureValue (ConstantTexture c) _ _ _ = c

data CheckerTexture f where
    CheckerTexture
      :: (Texture f even, Texture f odd)
      => even
      -> odd
      -> CheckerTexture f

instance (Floating f, Ord f) => Texture f (CheckerTexture f) where
    textureValue (CheckerTexture t1 t2) u v p =
        let sines = sin (10*p^._x) * sin (10*p^._y) * sin (10*p^._z)
            textureFun = if sines<0 then textureValue t1 else textureValue t2
        in textureFun u v p
