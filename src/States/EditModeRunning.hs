{-# LANGUAGE TupleSections #-}

module States.EditModeRunning where
#include "Utils.cpp"
import Control.Applicative ((<$>))
import qualified Graphics.UI.GLFW as GLFW
import Data.Composition ((.:))
import Gamgine.Math.Vect as V
import Gamgine.Control ((?))
import qualified Data.List as L
import qualified GameData.Player as PL
import qualified GameData.Enemy as EN
import qualified GameData.Entity as E
import qualified GameData.Star as S
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified Entity.Id as EI
import qualified Level.Reload as LR
import qualified Gamgine.State.State as ST
import qualified Gamgine.State.KeyInfo as KI
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG
import qualified States.GameRunning as GR
IMPORT_LENS_AS_LE


-- | the edit mode state
mkEditModeRunningState :: ST.State GD.Data
mkEditModeRunningState = ST.State {
   ST.enter      = (Just . (, mkEditModeRunningState)) .: flip const,
   ST.leave      = (, mkEditModeRunningState),
   ST.update     = (, mkEditModeRunningState) . GR.update,
   ST.render     = ((, mkEditModeRunningState) <$>) .: GR.render,
   ST.keyEvent   = (, mkEditModeRunningState) .: keyEvent,
   ST.mouseEvent = (, mkEditModeRunningState) .: flip const,
   ST.mouseMoved = (, mkEditModeRunningState) .: flip const
   }


keyEvent :: KI.KeyInfo -> GD.Data -> GD.Data
keyEvent ki@KI.KeyInfo {KI.key = key, KI.status = status, KI.mousePos = mp@(mpx:.mpy:.mpz:.())} gd =
   case (key, status) of
        (GLFW.Key'P, KI.Pressed) ->
           let starId  = LV.freeEntityId $ LE.getL GD.currentLevelL gd
               starPos = V.v3 (mpx - (fst S.starSize * 0.5)) (mpy - (snd S.starSize * 0.5)) 0
               in LE.modL (LV.entitiesL . GD.currentLevelL) (S.newStar starId starPos :) gd

        (GLFW.Key'E, KI.Pressed) ->
           let eneId  = LV.freeEntityId $ LE.getL GD.currentLevelL gd
               enePos = V.v3 (mpx - (fst EN.enemySize * 0.5)) (mpy - (snd EN.enemySize * 0.5)) 0
               in LE.modL (LV.entitiesL . GD.currentLevelL) (EN.newEnemy eneId (Left enePos):) gd

        (GLFW.Key'R, KI.Pressed) ->
           case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                Just e -> E.eFilter ((/= EI.entityId e) . EI.entityId) gd
                _      -> gd

        (GLFW.Key'A, KI.Pressed) ->
           GD.addEmptyLevel (shiftPressed ? GD.BeforeCurrent $ GD.AfterCurrent) gd

        (GLFW.Key'M, KI.Pressed) ->
           GD.moveCurrentLevel (shiftPressed ? GD.Forward $ GD.Backward) gd

        (GLFW.Key'N, KI.Pressed)
           | shiftPressed -> GD.toPreviousLevel gd
           | otherwise    -> GD.toNextLevel gd

        (GLFW.Key'L, KI.Pressed) ->
           LE.modL GD.currentLevelL LR.reload gd

        (GLFW.Key'Delete, KI.Pressed) ->
           GD.removeCurrentLevel gd

        _ -> GR.keyEvent ki gd

   where
      shiftPressed = GLFW.modifierKeysShift $ KI.modifiers ki
