
module Level.Update where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Event as EV
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified Entity.Update as EU
import qualified Entity.Intersect as EI
import qualified Level.ResolveIntersection as LR
IMPORT_LENS


update :: LV.Level -> ([EV.Event], LV.Level)
update level = (events, level')
   where
      events = catMaybes $ L.map LR.resolveIntersection isects
      isects = intersections level'
      level' = updateEntities level


updateEntities :: LV.Level -> LV.Level
updateEntities level = updateEntities_ levelGravity level
   where
      levelGravity = LE.getL (LY.gravityL . LV.activeLayerL) level

      updateEntities_ levelGravity level =
         level {LV.entities = L.map (EU.update $ EU.UpdateState levelGravity) $ LV.entities level,
                LV.layers   = updateLayerEntities <$> LV.layers level}

      updateLayerEntities layer@LY.Layer {LY.gravity = g} =
         layer {LY.entities = L.map (EU.update $ EU.UpdateState g) $ LY.entities layer}


intersections :: LV.Level -> [EI.Intersection]
intersections level =
   intersects levelEnts actLayerEnts ++ selfIntersects ([levelEnts, actLayerEnts] ++ inactLaysEnts)
   where
      selfIntersects = L.foldl' (\isects es -> isects ++ intersects es es) []

      intersects entities1 entities2 =
         catMaybes [EI.intersect e1 e2 | e1 <- entities1, e2 <- entities2]

      levelEnts     = LV.entities level
      actLayerEnts  = LE.getL (LY.entitiesL . LV.activeLayerL) level
      inactLaysEnts = L.map LY.entities $ LV.inactiveLayers level
