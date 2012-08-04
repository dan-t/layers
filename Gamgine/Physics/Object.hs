
module Gamgine.Physics.Object where
import Data.Lens.Strict
import Gamgine.Math.Vect
import Gamgine.Coroutine
import qualified Gamgine.Math.BoxTree as BT

#include "Gamgine/Utils.cpp"

data Mobility = Dynamic | Static deriving (Show, Eq)
data Position = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)
type Bound    = BT.BoxTree Position

-- attributes of a physical object, velocity in m/tick
data Object = Object {
   objectId :: Int,
   mobility :: Mobility,
   velocity :: Vect,
   position :: Vect,
   bound    :: Bound,
   update   :: Coroutine (Object, Double) Object
   }

LENS(objectId)
LENS(mobility)
LENS(velocity)
LENS(position)
LENS(bound)
LENS(update)
