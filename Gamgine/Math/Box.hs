
module Gamgine.Math.Box where
import Data.Maybe
import qualified Data.List as L
import Gamgine.Math.Vect as V
import Gamgine.Math.Utils
import Gamgine.Utils


-- axis aligned bounding box
data Box = Box {
   minPt :: Vect,
   maxPt :: Vect
   } deriving (Show, Read)

center :: Box -> Vect
center b = minPt b + halfs b

halfs :: Box -> Vect
halfs (Box minPt maxPt) = (maxPt - minPt) * 0.5

intersects :: Box -> Box -> Bool
b1 `intersects` b2 =
   not $ minPt b2 `greaterComp` maxPt b1 || maxPt b2 `lesserComp` minPt b1

moveBy :: Box -> Vect -> Box
(Box min max) `moveBy` v = Box (min + v) (max + v)

contains :: Box -> Vect -> Bool
contains b v =
   V.and (V.zipWith (\c1 c2 -> c1 >= c2) v (minPt b))
      && V.and (V.zipWith (\c1 c2 -> c1 <= c2) v (maxPt b))

bound :: [Box] -> Box
bound (b:bs) = L.foldr (\b1 b2 -> Box (minVec (minPt b1) (minPt b2))
                                      (maxVec (maxPt b1) (maxPt b2))) b bs

-- overlapping if distance negative in all dimensions
distance :: Box -> Box -> Vect
distance b1 b2 = (abs $ center b2 - center b1) - (halfs b1 + halfs b2)

-- If the boxes are overlapping, than minOverlap returns the minimal
-- distance in each dimension by which b1 has to be moved to resolve
-- the overlapping with b2. Otherwise, if not overlapping, for each
-- dimension 0 is returned.
minOverlap :: Box -> Box -> Vect
minOverlap b1 b2 =
   let min1 = minPt b1
       max1 = maxPt b1
       min2 = minPt b2
       max2 = maxPt b2
       in fromList $ L.map (\i -> overlap min1 max1 min2 max2 i) [0..2]
   where
      overlap :: Vect -> Vect -> Vect -> Vect -> Int -> Double
      overlap min1 max1 min2 max2 dim =
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

