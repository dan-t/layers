
module Entity.Entity where
import Control.Applicative ((<$>))
import qualified Gamgine.Math.BoxTree as BT
import qualified FileData.Data2 as FD
import qualified Event as EV

data Position = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)
type Bound    = BT.BoxTree Position

data Scope = LevelScope | ActiveLayerScope | InactiveLayerScope deriving (Show, Eq)

-- | the type class for all game entities
class EntityT e where
   update      :: e -> Scope -> e

   render      :: e -> Scope -> IO e

   handleEvent :: e -> EV.Event -> Scope -> e

   getBound    :: e -> Maybe Bound


-- | convert a game data entity into a file data entity
class ToFileEntity e where
   toFileEntity :: e -> FD.Entity


-- | wrapper which can carry any e which has an instance
--   for Show, ToFileEntity and EntityT
data Entity = forall e. (Show e, ToFileEntity e, EntityT e) => Entity e

instance Show Entity where
   show (Entity e) = show e
