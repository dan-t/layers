{-# LANGUAGE TupleSections #-}

module States.MovingEntity where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Gamgine.Math.Vect as V
import Gamgine.Control ((?))
import qualified States.State as ST
import qualified States.GameRunning as GR
import qualified GameData.Level as LV
import qualified GameData.Entity as E
import qualified GameData.Data as GD
import qualified Entity.Id as EI
import qualified Entity.Position as EP
IMPORT_LENS

data MovingEntity = MovingEntity {
   entityId :: Maybe Int,
   startPos :: V.Vect,
   basePos  :: V.Vect
   }

instance ST.State MovingEntity GD.Data where
   enterWithMousePos me gd mp =
      case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
           Just e -> (me {entityId = Just $ EI.entityId e,
                          startPos = mp,
                          basePos  = EP.position e}, gd)

   leave me gd = (me {entityId = Nothing, startPos = V.nullVec, basePos = V.nullVec}, gd)

   render me gd rs = (me,) <$> GR.render gd rs

   mouseMoved me@MovingEntity {entityId = Just id, startPos = sp, basePos = bp} gd mp =
      (me, E.eMap (\e -> id == EI.entityId e ? EP.setPosition e newPos $ e) gd)
      where
         newPos = bp + (mp - sp)

   mouseMoved me gd _ = (me, gd)
