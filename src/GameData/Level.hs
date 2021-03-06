
module GameData.Level where
#include "Utils.cpp"
import Control.Applicative ((<$>))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import qualified Gamgine.Utils as GU
import qualified Gamgine.Zipper as GZ
import Gamgine.Control (applyIf)
import qualified Gamgine.Math.Vect as V
import Gamgine.Math.Vect
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB
import qualified Entity.Id as EI
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Boundary as BD
import qualified Rendering.Renderer as RD
IMPORT_LENS_AS_LE


data Level = Level {
   boundary  :: BD.Boundary,
   renderers :: [RD.Renderer],
   entities  :: [E.Entity],
   layers    :: LZ.Zipper LY.Layer
   }

LENS(boundary)
LENS(renderers)
LENS(entities)
LENS(layers)


instance E.ApplyToEntity Level where
   eMap f level = level {entities = E.eMap f $ entities level,
                         layers   = E.eMap f <$> layers level}

   eFilter p level = level {entities = E.eFilter p $ entities level,
                            layers   = E.eFilter p <$> layers level}


activeLayerL    = activeLayerLens
activeLayerLens = LE.lens getActiveLayer setActiveLayer
   where
      getActiveLayer       = LZ.cursor . layers
      setActiveLayer layer = LE.modL layersL $ LZ.replace layer


inactiveLayers :: Level -> [LY.Layer]
inactiveLayers level = before ++ after
   where
      before = LZ.toList . GZ.before . layers $ level
      after  = LZ.toList . GZ.after  . layers $ level


newLevel :: [E.Entity] -> [LY.Layer] -> Level
newLevel entities layers = Level boundary [] entities $ LZ.fromList layers
   where
      boundary    = BD.newBoundary allEntities
      allEntities = entities ++ (L.concat $ L.map LY.entities $ layers)


newEmptyLevel :: Level
newEmptyLevel =
   Level {boundary  = BD.Boundary $ B.Box V.nullVec (V.v3 2000 1000 0),
          renderers = [],
          entities  = [PL.newPlayer 0 (V.v3 1 1 0)],
          layers    = LZ.fromList [LY.newEmptyLayer, LY.newEmptyLayer]}


changeLevels :: Level -> Level -> (Level, Level)
changeLevels fromLevel toLevel =
   case findEntity E.isPlayer fromLevel of
        Just E.Player {E.playerVelocity = (vx:._)} ->
           (fromLevel, E.eMap (applyIf E.isPlayer $ \p -> p {E.playerVelocity = (vx:.0:.0)}) toLevel)

        _ -> (fromLevel, toLevel)


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

findEntityById id = findEntity $ (== id) . EI.entityId

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


allStarsCollected :: Level -> Bool
allStarsCollected = L.all (== True) . L.map E.starCollected . L.filter E.isStar . allEntities


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
