{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RaytracingBook.Texture where

import Control.Lens
import Linear
import Linear.Affine
import qualified Codec.Picture as JP

class Texture f a | a -> f where
    textureValue :: a -> V2 f -> Point V3 f -> V3 Float

newtype ConstantTexture f =
    ConstantTexture (V3 Float)

instance Texture f (ConstantTexture f) where
    textureValue (ConstantTexture c) _ _ = c

data CheckerTexture f where
    CheckerTexture
      :: (Texture f even, Texture f odd)
      => even
      -> odd
      -> CheckerTexture f

instance (Floating f, Ord f) => Texture f (CheckerTexture f) where
    textureValue (CheckerTexture t1 t2) uv p =
        let sines = sin (10*p^._x) * sin (10*p^._y) * sin (10*p^._z)
            textureFun = if sines<0 then textureValue t1 else textureValue t2
        in textureFun uv p

data Imagetexture f =
    ImageTexture !(JP.Image JP.PixelRGBF)

instance RealFrac f => Texture f (Imagetexture f) where
    textureValue (ImageTexture image) uv _ =
        let nx = JP.imageWidth image
            ny = JP.imageHeight image
            i = min (floor (max (uv^._x*fromIntegral nx) 0)) (nx-1)
            j = min (floor (max ((1-uv^._y)*fromIntegral ny) 0)) (ny-1)
            JP.PixelRGBF r g b = JP.pixelAt image i j
        in V3 r g b
