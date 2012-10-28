
module GameData.Level where
#include "Gamgine/Utils.cpp"
import Data.Function (on)
import qualified Data.List as L
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB
import qualified Entity.Id as EI
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
IMPORT_LENS


data Level = Level {
   levelId        :: Int,
   entities       :: [E.Entity],
   activeLayer    :: LY.Layer,
   inactiveLayers :: [LY.Layer]
   } deriving Show

LENS(levelId)
LENS(entities)
LENS(activeLayer)
LENS(inactiveLayers)


instance E.ApplyToEntity Level where
   eMap f level = level {entities       = E.eMap f $ entities level,
                         activeLayer    = E.eMap f $ activeLayer level,
                         inactiveLayers = L.map (E.eMap f) $ inactiveLayers level}


newLevel :: Int -> [E.Entity] -> [LY.Layer] -> Level
newLevel id entites (actLay : inactLays) = Level id entites actLay inactLays


allLayers :: Level -> [LY.Layer]
allLayers Level {activeLayer = actLay, inactiveLayers = inactLays} =
   LY.sortById $ actLay : inactLays


switchToNextLayer :: Level -> Level
switchToNextLayer l@Level {activeLayer = actLay, inactiveLayers = inactLays} =
   l {activeLayer = L.head inactLays, inactiveLayers = L.tail inactLays ++ [actLay]}


sortById :: [Level] -> [Level]
sortById levels = L.sortBy (compare `on` levelId) levels


-- | the entities of the level and all its layers
allEntities :: Level -> [E.Entity]
allEntities Level {entities = entities, activeLayer = actLay, inactiveLayers = inactLays} =
   entities ++ (L.concat $ L.map LY.entities (actLay : inactLays))


findEntity :: (E.Entity -> Bool) -> Level -> Maybe E.Entity
findEntity f level = L.find f $ allEntities level


findEntityAt pos = findEntity $ \e -> (BT.asBox . EB.bound $ e) `B.contains` pos


data AddEntityTo = ToLevel | ToActiveLayer

addEntity :: E.Entity -> AddEntityTo -> Level -> Level
addEntity entity ToLevel       level = LE.modL entitiesL (entity :) level
addEntity entity ToActiveLayer level = LE.modL (LY.entitiesL . activeLayerL) (entity :) level


freeEntityId :: Level -> Int
freeEntityId level = maxId + 1
   where
      maxId | L.null allIds = 0
            | otherwise     = L.maximum allIds

      allIds = L.map EI.entityId (allEntities level)


-- | a lens for the player entity
playerL    = playerLens
playerLens = LE.lens getPlayer setPlayer
   where
      getPlayer = \level ->
         case findEntity E.isPlayer level of
              Just p -> p
              _      -> error $ "Couldn't find player!"

      setPlayer = \player level -> E.eMap (set player) level
         where
            set player E.Player {} = player
            set _      e           = e
