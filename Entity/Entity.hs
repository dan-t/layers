
module Entity.Entity where
import Control.Applicative ((<$>))
import qualified Gamgine.Math.BoxTree as BT
import qualified Event as EV

data Position = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)
type Bound    = BT.BoxTree Position

data Scope = LevelScope | ActiveLayerScope | InactiveLayerScope deriving (Show, Eq)

class EntityT e where
   update :: e -> Scope -> e

   render :: e -> Scope -> IO e

   handleEvent :: e -> EV.Event -> Scope -> e

   getBound :: e -> Maybe Bound


data Entity = forall e. (Show e, EntityT e) => Entity e

instance Show Entity where
   show (Entity e) = show e
