
module Gamgine.Math.Matrix (module Data.Vec,Matrix,mkOrtho,mkScale,inverseOrIdentity,worldToWinMatrix,orthoMatrix,windowMatrix) where
import Data.Vec
import Gamgine.Math.Vect

-- row major matrix
type Matrix = Mat44 Double

mkOrtho :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
mkOrtho left rigth bottom top near far =
   let rml = rigth - left
       rpl = rigth + left
       tmb = top - bottom
       tpb = top + bottom
       fmn = far - near
       fpn = far + near
       in matFromList [2 / rml,       0,       0, -(rpl / rml),
                       0      , 2 / tmb,       0, -(tpb / tmb),
		       0      ,       0, 2 / fmn, -(fpn / fmn),
		       0      ,       0,       0,            1]

mkScale :: Vect -> Matrix
mkScale v = scale (snoc v 1) identity 

mkTranslate :: Vect -> Matrix
mkTranslate v = translate v identity

inverseOrIdentity :: Matrix -> Matrix
inverseOrIdentity m =
   case invert m of
	Nothing -> identity
	Just m  -> m

worldToWinMatrix :: Double -> Double -> Double -> Matrix
worldToWinMatrix width height orthoTop = windowMatrix width height `multmm` orthoMatrix (width/height) orthoTop

orthoMatrix :: Double -> Double -> Matrix
orthoMatrix aspectRatio top = mkOrtho 0 (top * aspectRatio) 0 top (-1) 1

windowMatrix :: Double -> Double -> Matrix
windowMatrix width height = toGLFW height `multmm` unitCubeToWin width height
   where
      unitCubeToWin :: Double -> Double -> Matrix
      unitCubeToWin width height = 
	 mkScale (v3 (width*0.5) (height*0.5) 0.5) `multmm` mkTranslate (v3 1 1 1)

      toGLFW :: Double -> Matrix
      toGLFW height = mkTranslate (v3 0 height 0) `multmm` mkScale (v3 1 (-1) 1) 

