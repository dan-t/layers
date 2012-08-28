{-# LANGUAGE ExistentialQuantification #-}

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
   initRessources :: e -> IO e
   initRessources e = return e

   -- | update the properties of the entity e.g. it's position
   update :: Scope -> e -> e
   update _ e = e

   -- | render the current state of the entity
   render :: Scope -> e -> IO ()
   render _ _ = return ()

   -- | handle a event send to the entity
   handleEvent :: Scope -> EV.Event -> e -> e
   handleEvent _ _ e = e

   -- | returns the bound of the entity, used for collision detection
   getBound :: e -> Maybe Bound
   getBound _ = Nothing


-- | convert a game data entity into a file data entity
class ToFileEntity e where
   toFileEntity :: e -> FD.Entity


-- | wrapper which can carry any e which has an instance
--   for Show, ToFileEntity and EntityT
data Entity = forall e. (Show e, ToFileEntity e, EntityT e) => Entity e


instance EntityT Entity where
   initRessources (Entity e) = Entity <$> initRessources e

   update scope (Entity e) = Entity $ update scope e

   render scope (Entity e) = render scope e

   handleEvent scope event (Entity e) = Entity $ handleEvent scope event e

   getBound (Entity e) = getBound e


instance Show Entity where
   show (Entity e) = show e
