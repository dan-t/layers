
module Gamgine.Math.Box where
#include "Gamgine/Utils.cpp"
import Data.Maybe
import qualified Data.List as L
import Gamgine.Math.Vect as V
import Gamgine.Math.Utils
import Gamgine.Utils
IMPORT_LENS


-- axis aligned bounding box
data Box = Box {
   minPt :: Vect,
   maxPt :: Vect
   } deriving (Show, Read)

LENS(minPt)
LENS(maxPt)

center :: Box -> Vect
center b = minPt b + halfs b

halfs :: Box -> Vect
halfs (Box minPt maxPt) = (maxPt - minPt) * 0.5

intersects :: Box -> Box -> Bool
Box min1 max1 `intersects` Box min2 max2 =
   not $ V.any (>) min2 max1 || V.any (<) max2 min1

inside :: Box -> Box -> Bool
Box min1 max1 `inside` Box min2 max2 =
   V.all (>=) min1 min2 && V.all (<=) max1 max2

moveBy :: Box -> Vect -> Box
Box min max `moveBy` v = Box (min + v) (max + v)

extendBy :: Box -> Box -> Box
Box min1 max1 `extendBy` Box min2 max2 =
   Box (V.minVec min1 min2) (V.maxVec max1 max2)

contains :: Box -> Vect -> Bool
contains (Box min max) v =
   V.all (>=) v min && V.all (<=) v max

bound :: [Box] -> Box
bound []     = Box (V.v3 0 0 0) (V.v3 0 0 0)
bound (b:bs) = L.foldr extendBy b bs

-- overlapping if distance negative in all dimensions
distance :: Box -> Box -> Vect
distance b1 b2 = (abs $ center b2 - center b1) - (halfs b1 + halfs b2)

-- If the boxes are overlapping, than minOverlap returns the minimal
-- distance in each dimension by which b1 has to be moved to resolve
-- the overlapping with b2. Otherwise, if not overlapping, for each
-- dimension 0 is returned.
minOverlap :: Box -> Box -> Vect
minOverlap (Box min1 max1) (Box min2 max2) = V.fromList $ L.map overlap [0..2]
   where
      overlap dim =
	 let v1@(minv1, maxv1) = (getElem dim min1, getElem dim max1)
             v2@(minv2, maxv2) = (getElem dim min2, getElem dim max2)
             in if maxv1 < minv2 || minv1 > maxv2
                   then 0
                   else let minv1Outside     = minv1 < minv2
                            maxv1Outside     = maxv1 > maxv2
                            v1Inside         = not minv1Outside && not maxv1Outside
                            o | v1Inside     = insideOverlap v1 v2
                              | minv1Outside = minOutsideOverlap v1 v2
                              | maxv1Outside = maxOutsideOverlap v1 v2
                            in o

      insideOverlap (minv1, maxv1) (minv2, maxv2) =
         let leftDist  = maxv1 - minv2
             rightDist = maxv2 - minv1
             o | leftDist < rightDist = -leftDist
               | otherwise            =  rightDist
             in o

      minOutsideOverlap (_, maxv1) (minv2, _) = -(maxv1 - minv2)
      maxOutsideOverlap (minv1, _) (_, maxv2) = maxv2 - minv1


type Tuple3d = (Double,Double,Double)

fromTuples :: (Tuple3d, Tuple3d) -> Box
fromTuples (t1, t2) = Box (V.fromTuple t1) (V.fromTuple t2)

toTuples :: Box -> (Tuple3d, Tuple3d)
toTuples (Box minPt maxPt) = (V.toTuple minPt, V.toTuple maxPt)
