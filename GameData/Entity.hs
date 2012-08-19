
module GameData.Entity where
import Control.Applicative ((<$>))
import qualified Gamgine.Math.BoxTree as BT
import qualified FileData.Data2 as FD
import qualified Event as EV

-- | for classification of a box inside the box tree
data Position = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)

-- | the bound of an entity
type Bound = BT.BoxTree Position

-- | the scope of the entity
data Scope = LevelScope         -- ^ entity is part of all layers
           | ActiveLayerScope   -- ^ entity is part of the currently active layer
           | InactiveLayerScope -- ^ entity is part of a currently inactive layer
           deriving (Show, Eq)

-- | the type class for all game entities
class EntityT e where
   -- | init ressources of entity e.g. load textures
   initRessources :: e -> Scope -> IO e
   initRessources e _ = return e

   -- | update the properties of the entity e.g. it's position
   update :: e -> Scope -> e
   update e _ = e

   -- | render the current state of the entity
   render :: e -> Scope -> IO ()
   render e _ = return ()

   -- | handle a event send to the entity
   handleEvent :: e -> EV.Event -> Scope -> e
   handleEvent e _ _ = e

   -- | returns the bound of the entity, used for collision detection
   getBound :: e -> Maybe Bound
   getBound _ = Nothing


-- | convert a game data entity into a file data entity
class ToFileEntity e where
   toFileEntity :: e -> FD.Entity


-- | wrapper which can carry any e which has an instance
--   for Show, ToFileEntity and EntityT
data Entity = forall e. (Show e, ToFileEntity e, EntityT e) => Entity e

instance Show Entity where
   show (Entity e) = show e
