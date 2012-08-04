
module GameData.Player where
import qualified GameData.PhysicalAttrs as P
import Gamgine.Math.Vect

data Movement = ToTheLeft | ToTheRight | AtRest

data Player = Player {
   id              :: Int,
   initialPosition :: Vect,
   physicalAttrs   :: P.PhysicalAttrs,
   onBottom        :: Bool,
   movement        :: Movement
   }
