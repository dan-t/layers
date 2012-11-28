{-# LANGUAGE TupleSections #-}

module States.EditModeRunning where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Graphics.UI.GLFW as GLFW
import Data.Composition ((.:))
import Gamgine.Math.Vect as V
import qualified Data.List as L
import qualified GameData.Player as PL
import qualified GameData.Entity as E
import qualified GameData.Star as S
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified Entity.Id as EI
import qualified States.State as ST
import qualified States.KeyInfo as KI
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG
import qualified States.GameRunning as GR
IMPORT_LENS


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
        (GLFW.CharKey 'P', KI.Pressed) ->
           let starId  = LV.freeEntityId $ LE.getL GD.currentLevelL gd
               starPos = V.v3 (mpx - (fst S.starSize * 0.5)) (mpy - (snd S.starSize * 0.5)) 0
               in LE.modL (LV.entitiesL . GD.currentLevelL) (S.newStar starId starPos :) gd

        (GLFW.CharKey 'R', KI.Pressed) ->
           case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                Just e -> E.eFilter ((/= EI.entityId e) . EI.entityId) gd
                _      -> gd

        (GLFW.CharKey 'A', KI.Pressed) ->
           GD.addEmptyLevel gd

        (GLFW.CharKey 'N', KI.Pressed)
           | L.any (== KI.Shift) (KI.modifiers ki) -> GD.toPreviousLevel gd
           | otherwise                             -> GD.toNextLevel gd

        (GLFW.CharKey 'L', KI.Pressed) ->
           LE.modL GD.currentLevelL (TG.toLevel . TF.toLevel) gd

        _ -> GR.keyEvent ki gd
