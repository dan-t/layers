
module Callback.Key where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.IORef as R
import Control.Monad (liftM2)
import Control.Applicative ((<$>))
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import Gamgine.IORef as GR
import Gamgine.Utils (ifJust, applyIf, applyIfM)
import qualified Utils as LU
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Star as S
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified Entity.Id as EI
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG
IMPORT_LENS

type Pressed     = Bool
type KeyCallback = (GLFW.Key -> Pressed -> IO ())

newKeyCallback :: AP.AppDataRef -> AP.AppMode -> KeyCallback
newKeyCallback appDataRef _ = callback
   where
      callback GLFW.KeyLeft       True  = accelerateToTheLeft
      callback GLFW.KeyLeft       False = accelerateToTheRight
      callback GLFW.KeyRight      True  = accelerateToTheRight
      callback GLFW.KeyRight      False = accelerateToTheLeft
      callback GLFW.KeyUp         True  = jump
      callback GLFW.KeyTab        True  = switchToNextLayer
      callback (GLFW.CharKey 'P') True  = placeStar
      callback (GLFW.CharKey 'R') True  = removeEntity
      callback (GLFW.CharKey 'A') True  = addEmptyLevel

      callback (GLFW.CharKey 'N') True  = do
         shiftPressed <- anyShiftKeyPressed
         if shiftPressed
            then toPreviousLevel
            else toNextLevel

      callback (GLFW.CharKey 'L') True  = reloadCurrentLevel

      callback _ _                     = return ()

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      jump = updateEntity (applyIf (\e -> E.isPlayer e && E.playerOnBottom e) $ \e ->
         e {E.playerVelocity = V.setElem 1 PL.jumpAcceleration $ E.playerVelocity e,
            E.playerOnBottom = False})


      switchToNextLayer = modL AP.currentLevelL LV.switchToNextLayer

      placeStar = do
         (mx:.my:._) <- mousePosition
         starId      <- LV.freeEntityId <$> getL AP.currentLevelL
         let starPos = V.v3 (mx - (fst S.starSize * 0.5)) (my - (snd S.starSize * 0.5)) 0
         modL (LV.entitiesL . AP.currentLevelL) $ (S.newStar starId starPos :)


      removeEntity = do
         mousePos <- mousePosition
         entity   <- LV.findEntityAt mousePos <$> getL AP.currentLevelL
         ifJust entity $ applyIfM (not . E.isPlayer) $ \e ->
            modL AP.currentLevelL (E.eFilter ((/= EI.entityId e) . EI.entityId))


      addEmptyLevel   = modL AP.gameDataL GD.addEmptyLevel
      toNextLevel     = modL AP.gameDataL GD.toNextLevel
      toPreviousLevel = modL AP.gameDataL GD.toPreviousLevel

      reloadCurrentLevel = modL AP.currentLevelL $ TG.toLevel . TF.toLevel


      updatePlayerVelocity f = updateEntity (applyIf E.isPlayer $ LE.modL E.playerVelocityL f)

      mousePosition  = do
         app <- R.readIORef appDataRef
         LU.mousePosInWorldCoords app


      updateEntity f = modL AP.currentLevelL $ E.eMap f
      modL           = GR.modL appDataRef
      getL           = GR.getL appDataRef

      anyCtrlKeyPressed  = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftCtrl)  (GLFW.keyIsPressed GLFW.KeyRightCtrl)
      anyShiftKeyPressed = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftShift) (GLFW.keyIsPressed GLFW.KeyRightShift)
