{-# LANGUAGE TupleSections #-}

module States.MovingEntity where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Gamgine.Math.Vect as V
import Gamgine.Control (applyIf, (?))
import qualified States.State as ST
import qualified States.GameRunning as GR
import qualified GameData.Level as LV
import qualified GameData.Entity as E
import qualified Entity.Id as EI
import qualified Entity.Position as EP
import qualified AppData as AP
IMPORT_LENS

data MovingEntity = MovingEntity {
   entityId :: Maybe Int,
   startPos :: V.Vect,
   basePos  :: V.Vect
   }

instance ST.State MovingEntity AP.AppData where
   enterWithMousePos me app mp =
      case LV.findEntityAt mp $ LE.getL AP.currentLevelL app of
           Just e -> (me {entityId = Just $ EI.entityId e,
                          startPos = mp,
                          basePos  = EP.position e}, app)

   leave me app = (me {entityId = Nothing, startPos = V.nullVec, basePos = V.nullVec}, app)

   render me app rs = (me,) <$> GR.render app rs

   mouseMoved me@MovingEntity {entityId = Just id, startPos = sp, basePos = bp} app mp =
      (me, E.eMap (\e -> id == EI.entityId e ? EP.setPosition e newPos $ e) app)
      where
         newPos = bp + (mp - sp)

   mouseMoved me app _ = (me, app)
