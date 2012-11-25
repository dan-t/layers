{-# LANGUAGE TupleSections #-}

module States.MovingEntity where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Composition ((.:))
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

-- | the state for moving an entity during edit mode
mkMovingEntityState :: ST.State GD.Data
mkMovingEntityState =
   mkState $ MovingEntity Nothing V.nullVec V.nullVec
   where
      mkState me = ST.State {
         ST.enter = \mp gd ->
            case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                 Just e -> (gd, mkState (me {entityId = Just $ EI.entityId e,
                                             startPos = mp,
                                             basePos  = EP.position e}))
                 _      -> (gd, mkState me),


         ST.leave = (, mkState (me {entityId = Nothing, startPos = V.nullVec, basePos = V.nullVec})),

         ST.update = (, mkState me) . GR.update,

         ST.render = ((, mkState me) <$>) .: GR.render,

         ST.keyEvent = (, mkState me) .: flip const,

         ST.mouseEvent = (, mkState me) .: flip const,

         ST.mouseMoved = \mp gd ->
            case me of
                 MovingEntity {entityId = Just id, startPos = sp, basePos = bp} ->
                    (E.eMap (\e -> id == EI.entityId e ? EP.setPosition e (bp + (mp - sp)) $ e) gd,
                     mkState me)

                 _ -> (gd, mkState me)
         }
