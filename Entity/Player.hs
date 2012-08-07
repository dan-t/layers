
module Entity.Player where
import qualified Gamgine.Math.Vect as V
import qualified Event as EV
import qualified Entity.Entity as E

data Movement = ToTheLeft | ToTheRight | AtRest deriving (Show, Eq)

data Player = Player {
   playerId        :: Int,
   initialPosition :: V.Vect,
   position        :: V.Vect,
   velocity        :: V.Vect,
   onBottom        :: Bool,
   movement        :: Movement
   } deriving Show

instance E.EntityT Player where
   update e s = e
   render e s = return e
   handleEvent e ev s = e
   getBound e = Nothing

newPlayer :: Int -> V.Vect -> Player
newPlayer id initPos = Player id initPos initPos (V.v3 0 0 0) False AtRest
