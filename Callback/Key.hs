
module Callback.Key where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.IORef as R
import Control.Applicative ((<$>))
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import Gamgine.IORef as GR
import Gamgine.Utils as GU
import qualified Utils as LU
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Star as S
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified Entity.Id as EI
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

      callback (GLFW.CharKey 'P') True = placeStar

      callback (GLFW.CharKey 'R') True = removeEntity

      callback _ _                     = return ()

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      jump = updateEntity (GU.applyIf (\e -> E.isPlayer e && E.playerOnBottom e) $ \e ->
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
         GU.just entity $ GU.applyIfM (not . E.isPlayer) $ \e ->
            modL AP.currentLevelL (E.eFilter ((/= EI.entityId e) . EI.entityId))


      updatePlayerVelocity f = updateEntity (GU.applyIf E.isPlayer $ LE.modL E.playerVelocityL f)

      mousePosition  = do app <- R.readIORef appDataRef
                          LU.mousePosInWorldCoords app

      updateEntity f = modL AP.currentLevelL $ E.eMap f
      modL           = GR.modL appDataRef
      getL           = GR.getL appDataRef
