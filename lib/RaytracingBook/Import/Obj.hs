module RaytracingBook.Import.Obj where

import RaytracingBook.Hitable
import RaytracingBook.Triangle

import Codec.Wavefront hiding (Triangle)
import Linear
import Linear.Affine
import qualified Data.Vector as V

{-# SPECIALIZE fromWavefrontOBJ :: WavefrontOBJ -> Material Float -> V.Vector (Triangle Float) #-}
{-# SPECIALIZE fromWavefrontOBJ :: WavefrontOBJ -> Material Double -> V.Vector (Triangle Double) #-}
fromWavefrontOBJ :: Fractional f => WavefrontOBJ -> Material f -> V.Vector (Triangle f)
fromWavefrontOBJ obj mat =
    do e <- objFaces obj
       let Face fi1 fi2 fi3 fiN = elValue e
       let locs = map ((objLocations obj V.!) . (+ (-1)) . faceLocIndex) (fi1:fi2:fi3:fiN)
       let (l1:locs') = map (\l -> fmap realToFrac $ P (V3 (locX l) (locY l) (locZ l))) locs
       let locPairs = zip locs' (tail locs')
       (l2,l3) <- V.fromList locPairs
       pure $ Triangle (V3 l1 l2 l3) mat
