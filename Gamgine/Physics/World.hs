
module Gamgine.Physics.World where
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Lens.Strict
import qualified Data.HashMap.Strict as HM
import Gamgine.Physics.Object
import Gamgine.Physics.ObjectUpdate as OU
import Gamgine.Math.Vect
import Gamgine.Utils
import Gamgine.Coroutine
#include "Gamgine/Utils.cpp"
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Utils   as U
import qualified Data.List as L

leaf1 = BT.leaf1
leaf2 = BT.leaf2

type ObjectId      = Int
type Intersections = [BT.Intersection Position]
type Collisions    = [(ObjectId, ObjectId, Intersections)]

-- attributes of the physical world
-- gravity in m/tick
data World = World {
   worldId           :: Int,
   objects           :: [Object],
   gravity           :: Double
   }

LENS(worldId)
LENS(objects)
LENS(gravity)

-- gravity in m/tick
makeWorld :: Int -> Double -> World
makeWorld id gravity =
   World id [] gravity

emptyWorld :: World
emptyWorld = World 0 [] 0


type ObjectLens = Lens World (Maybe Object)

-- make a lense for an object with id
objectL :: Int -> ObjectLens
objectL objId = lens (\w    -> getObject w objId)
                     (\mo w -> maybe w
                                     (\o -> updateObject w objId (\_ -> o))
                                     mo)


-- position and bound in m
addStaticObject :: World -> Int -> (Vect, Bound) -> World
addStaticObject world id (position, bound) =
   let objs  = objects $ removeObject world id
       in world {objects = (makeStaticObject id position bound) : objs}


makeStaticObject :: Int -> Vect -> Bound -> Object
makeStaticObject id position bound =
   Object id Static (v3 0 0 0) position bound makeIdentityObjectUpdate


-- velocity in m/tick, position and bound in m
addDynamicObject :: World -> Int -> (Vect, Vect, Bound, OU.ObjectUpdate) -> World
addDynamicObject world id (velocity, position, bound, update) =
   let objs  = objects $ removeObject world id
       in world {objects = (makeDynamicObject id velocity position bound update) : objs}


makeDynamicObject :: Int -> Vect -> Vect -> Bound -> OU.ObjectUpdate -> Object
makeDynamicObject id velocity position bound update =
   Object id Dynamic velocity position bound update


addObject :: World -> Object -> World
addObject w o = w {objects = o : objects w}


removeObject :: World -> Int -> World
removeObject world objId =
   world {objects = filter (\o -> objectId o /= objId) (objects world)}


updateObject :: World -> Int -> (Object -> Object) -> World
updateObject world objId update =
   world {objects = L.map (\o -> objectId o == objId ? update o $ o) (objects world)}


getObject :: World -> Int -> Maybe Object
getObject world objId =
   L.find (\o -> objectId o == objId) (objects world)


findObjectAt :: World -> Vect -> Maybe Object
findObjectAt world pos =
   L.find (\o -> let b = (BT.asBox $ bound o) `B.moveBy` position o
                     in b `B.contains` pos) (objects world)


objectAttribute :: (Object -> a) -> World -> Int -> Maybe a
objectAttribute getter world objId = do
   o <- getObject world objId
   return $ getter o


makeWorldTick :: World -> (World, Maybe Collisions)
makeWorldTick world =
   let objs' = updateDynamicObjects (gravity world) (objects world)
       cols  = computeCollisions objs'
       in (world {objects = objs'}, cols)


updateDynamicObjects :: Double -> [Object] -> [Object]
updateDynamicObjects gravity objs =
   L.map (\o ->
      if mobility o == Dynamic
         then let (o', up') = runCoroutine (update o) (o, gravity)
                  in o' {update = up'}
         else o) objs


computeCollisions :: [Object] -> Maybe Collisions
computeCollisions objects =
   case traverse objects [] of
        []     -> Nothing
        isects -> Just isects

   where
      traverse []     isects = isects
      traverse (o:[]) isects = isects
      traverse (o:os) isects = traverse os (intersect o os isects)

      intersect o [] isects = isects
      intersect o os isects =
         L.foldr (\oso isects ->
            if mobility o == Dynamic || mobility oso == Dynamic
               then let isect = isectBounds o oso
                        in not (L.null isect) ? (objectId o, objectId oso, isect) : isects $ isects
               else isects) isects os

      isectBounds o1 o2 = movedBound o1 o2 `BT.intersection` bound o2
      movedBound o1 o2  = (bound o1) `BT.moveBy` (position o1 - position o2)
