
module KeyCallback where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Event as EV
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
IMPORT_LENS

type Pressed     = Bool
type KeyCallback = (GLFW.Key -> Pressed -> IO ())

newKeyCallback :: AP.AppDataRef -> AP.AppMode -> KeyCallback
newKeyCallback appDataRef _ = callback
   where
      callback GLFW.KeyLeft True   = accelerateToTheLeft

      callback GLFW.KeyLeft False  = accelerateToTheRight

      callback GLFW.KeyRight True  = accelerateToTheRight

      callback GLFW.KeyRight False = accelerateToTheLeft

      callback GLFW.KeyUp True     = jump

      callback GLFW.KeyTab True    = switchToNextLayer

      callback _ _                 = return ()


      switchToNextLayer = updateAppEvent $ \app ->
         let layerIds   = L.map LY.layerId $ LE.getL (LV.layersL . AP.currentLevelL) app
             actLayId   = AP.activeLayerId app
             greaterIds = L.filter (> actLayId) layerIds
             smallerIds = L.filter (< actLayId) layerIds
             newLayId | not . null $ greaterIds = L.head greaterIds
                      | not . null $ smallerIds = L.head smallerIds
                      | otherwise               = actLayId

             in app {AP.activeLayerId = newLayId}


      jump = updateEntityEvent $ \e ->
         case e of
              E.Player {E.playerVelocity = vx:.vy:.vz:.(), E.playerOnBottom = True} ->
                 e {E.playerVelocity = V.v3 vx PL.jumpAcceleration vz, E.playerOnBottom = False}

              _ -> e

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      updatePlayerVelocity f = updateEntityEvent $ \e ->
         case e of
              E.Player {E.playerVelocity = v} -> e {E.playerVelocity = f v}
              _                               -> e

      updateEntityEvent   = sendEvent . mkUpdateEntityEvent
      updateAppEvent      = sendEvent . mkUpdateAppEvent
      mkUpdateEntityEvent = EV.MkEntityEvent . EV.UpdateEntity
      mkUpdateAppEvent    = EV.MkAppEvent . EV.UpdateApp
      sendEvent e         = EV.handleEventIO e appDataRef
