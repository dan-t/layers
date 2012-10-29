
module GameData.Level where
#include "Gamgine/Utils.cpp"
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import qualified Gamgine.Utils as GU
import qualified Gamgine.Zipper as GZ
import Gamgine.Utils (applyIf)
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB
import qualified Entity.Id as EI
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
import qualified GameData.Player as PL
IMPORT_LENS


data Level = Level {
   entities :: [E.Entity],
   layers   :: LZ.Zipper LY.Layer
   } deriving Show

LENS(entities)
LENS(layers)


instance E.ApplyToEntity Level where
   eMap f level = level {entities = E.eMap f $ entities level,
                         layers   = GZ.map (E.eMap f) $ layers level}

   eFilter p level = level {entities = E.eFilter p $ entities level,
                            layers   = GZ.map (E.eFilter p) $ layers level}


activeLayerL    = activeLayerLens
activeLayerLens = LE.lens getActiveLayer setActiveLayer
   where
      getActiveLayer       = LZ.cursor . layers
      setActiveLayer layer = LE.modL layersL $ LZ.replace layer


inactiveLayers :: Level -> [LY.Layer]
inactiveLayers level = before ++ after
   where
      (before, _, after) = GZ.split . layers $ level


newLevel :: [E.Entity] -> [LY.Layer] -> Level
newLevel entites layers = Level entites $ LZ.fromList layers


newEmptyLevel :: Level
newEmptyLevel =
   Level {entities = [PL.newPlayer 0 (V.v3 1 1 0)],
          layers   = LZ.fromList [LY.newEmptyLayer, LY.newEmptyLayer]}


allLayers :: Level -> [LY.Layer]
allLayers = LZ.toList . layers


toNextLayer :: Level -> Level
toNextLayer = LE.modL layersL $ \lays -> applyIf LZ.endp LZ.start $ LZ.right lays


-- | the entities of the level and all its layers
allEntities :: Level -> [E.Entity]
allEntities Level {entities = entities, layers = layers} =
   entities ++ (L.concat $ L.map LY.entities $ LZ.toList layers)


findEntity :: (E.Entity -> Bool) -> Level -> Maybe E.Entity
findEntity f level = L.find f $ allEntities level


findEntityAt pos = findEntity $ \e -> (BT.asBox . EB.bound $ e) `B.contains` pos


data AddEntityTo = ToLevel | ToActiveLayer

addEntity :: E.Entity -> AddEntityTo -> Level -> Level
addEntity entity ToLevel       = LE.modL entitiesL (entity :)
addEntity entity ToActiveLayer = LE.modL (LY.entitiesL . activeLayerL) (entity :)


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
