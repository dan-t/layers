module Gamgine.Math.Vect (module Data.Vec,Vect,Vect4,x,y,z,v3,v4,fromTuple,toTuple,fromVect4,len,
                          inverseVec,clampVec,maxVec,minVec,index,absVec,and,or,all,any) where

import Gamgine.Utils
#include "Gamgine/Utils.cpp"
import qualified Gamgine.Math.Utils as U
import Data.Vec
import qualified Data.Vec as V
import Prelude hiding (and, or, all, any)
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

inverseVec v = V.map (* (-1)) v

index :: (Double -> Bool) -> Vect -> Int
index f (x:.y:.z:.())
   | f x = 0
   | f y = 1
   | f z = 2
   | otherwise = ERROR "Couldn't find elem!"

clampVec :: Vect -> Vect -> Vect -> Vect
clampVec (minX:.minY:.minZ:.()) (maxX:.maxY:.maxZ:.()) (x:.y:.z:.()) =
   v3 (U.clamp minX maxX x) (U.clamp minY maxY y) (U.clamp minZ maxZ z)

maxVec v1 v2 = V.zipWith max v1 v2

minVec v1 v2 = V.zipWith min v1 v2

absVec v = V.map abs v

and v = V.foldr (&&) True v

or v = V.foldr (||) False v

all f v1 v2 = and $ V.zipWith f v1 v2

any f v1 v2 = or $ V.zipWith f v1 v2
