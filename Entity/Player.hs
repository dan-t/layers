
module Entity.Player where
import qualified Gamgine.Math.Vect as V

data Movement = ToTheLeft | ToTheRight | AtRest

data Player = Player {
   playerId        :: Int,
   initialPosition :: V.Vect,
   position        :: V.Vect,
   velocity        :: V.Vect,
   onBottom        :: Bool,
   movement        :: Movement
   }

newPlayer :: Int -> V.Vect -> Player
newPlayer id initPos = Player id initPos initPos (V.v3 0 0 0) False AtRest


update e s = e
render e s = return e
handleEvent e ev s = e
getBound e = undefined
