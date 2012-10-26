
module GameData.Level where
#include "Gamgine/Utils.cpp"
import Data.Function (on)
import qualified Data.List as L
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
IMPORT_LENS


data Level = Level {
   levelId  :: Int,
   entities :: [E.Entity],
   layers   :: [LY.Layer]
   } deriving Show

LENS(levelId)
LENS(entities)
LENS(layers)


sortById :: [Level] -> [Level]
sortById levels = L.sortBy (compare `on` levelId) levels


-- | the entities of the level and all its layers
allEntities :: Level -> [E.Entity]
allEntities Level {entities = entities, layers = layers} =
   entities ++ (L.concat $ L.map LY.entities layers)


findEntity :: (E.Entity -> Bool) -> Level -> Maybe E.Entity
findEntity f level = L.find f $ allEntities level


findEntityAt pos = findEntity $ \e -> (BT.asBox . EB.bound $ e) `B.contains` pos


playerL    = playerLens
playerLens = LE.lens getPlayer setPlayer
   where
      getPlayer = \level ->
         case findEntity isPlayer level of
              Just p -> p
              _      -> error $ "Couldn't find player!"
         where
            isPlayer E.Player {} = True
            isPlayer _           = False

      setPlayer = \player level -> E.eMap (set player) level
         where
            set player E.Player {} = player
            set _      e           = e


nextFreeEntityId :: Level -> Int
nextFreeEntityId level = maxId + 1
   where
      maxId | L.null allIds = 0
            | otherwise     = L.maximum allIds

      allIds = L.map E.entityId (allEntities level)


instance E.ApplyToEntity Level where
   eMap f level = level {entities = E.eMap f $ entities level,
                         layers   = L.map (E.eMap f) $ layers level}
