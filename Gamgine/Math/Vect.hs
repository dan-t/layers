module Gamgine.Math.Vect (module Data.Vec,Vect,Vect4,x,y,z,v3,v4,fromTuple,toTuple,fromVect4,len,
                          inverseVec,clampVec,maxVec,minVec,greaterComp,lesserComp,index,
			  absVec,and) where

import Gamgine.Utils
#include "Gamgine/Utils.cpp"
import qualified Gamgine.Math.Utils as U
import Data.Vec
import qualified Data.Vec as V
import Prelude hiding (and)
import Debug.Trace

type Vect  = Vec3 Double
type Vect4 = Vec4 Double

x :: Vect -> Double
x (x:._) = x

y :: Vect -> Double
y (_:.y:._) = y

z :: Vect -> Double
z (_:._:.z:.()) = z

v3 :: Double -> Double -> Double -> Vect
v3 x y z = x:.y:.z:.()

v4 :: Double -> Double -> Double -> Double -> Vect4
v4 x y z w = x:.y:.z:.w:.()

fromTuple :: (Double,Double,Double) -> Vect
fromTuple (x,y,z) = v3 x y z

toTuple :: Vect -> (Double,Double,Double)
toTuple (x:.y:.z:.()) = (x,y,z)

fromVect4 :: Vect4 -> Vect
fromVect4 (x:.y:.z:.w:.()) = v3 x y z

len :: Vect -> Double
len = norm

inverseVec :: Vect -> Vect
inverseVec (x:.y:.z:.()) = v3 (-x) (-y) (-z)

index :: (Double -> Bool) -> Vect -> Int
index f (x:.y:.z:.())
   | f x = 0
   | f y = 1
   | f z = 2
   | otherwise = ERROR "Couldn't find elem!"

clampVec :: Vect -> Vect -> Vect -> Vect
clampVec (minX:.minY:.minZ:.()) (maxX:.maxY:.maxZ:.()) (x:.y:.z:.()) =
   v3 (U.clamp minX maxX x) (U.clamp minY maxY y) (U.clamp minZ maxZ z)

maxVec :: Vect -> Vect -> Vect
maxVec (x0:.y0:.z0:.()) (x1:.y1:.z1:.()) = v3 (max x0 x1) (max y0 y1) (max z0 z1)

minVec :: Vect -> Vect -> Vect
minVec (x0:.y0:.z0:.()) (x1:.y1:.z1:.()) = v3 (min x0 x1) (min y0 y1) (min z0 z1)

absVec :: Vect -> Vect
absVec (x:.y:.z:.()) = v3 (abs x) (abs y) (abs z)

and :: Vec3 Bool -> Bool
and v = V.foldr (\c1 c2 -> c1 && c2) True v

-- if a component of the first vector is greater
-- than a component of the second one
greaterComp :: Vect -> Vect -> Bool
(x0:.y0:.z0:.()) `greaterComp` (x1:.y1:.z1:.()) = x0 > x1 || y0 > y1 || z0 > z1

-- if a component of the first vector is lesser
-- than a component of the second one
lesserComp :: Vect -> Vect -> Bool
(x0:.y0:.z0:.()) `lesserComp` (x1:.y1:.z1:.()) = x0 < x1 || y0 < y1 || z0 < z1
