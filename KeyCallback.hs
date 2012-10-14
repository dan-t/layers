
module KeyCallback where

import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Event as EV
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Layer as LY
import qualified GameData.Level as LV


newKeyCallback :: AP.AppDataRef -> (GLFW.Key -> Bool -> IO ())
newKeyCallback appDataRef = keyCallback
   where
      keyCallback GLFW.KeyLeft True = accelerateToTheLeft

      keyCallback GLFW.KeyLeft False = accelerateToTheRight

      keyCallback GLFW.KeyRight True = accelerateToTheRight

      keyCallback GLFW.KeyRight False = accelerateToTheLeft

      keyCallback GLFW.KeyUp True = sendEvent . mkUpdateEntityEvent $ \e ->
         case e of
              E.Player {E.playerVelocity = vx:.vy:.vz:.(), E.playerOnBottom = True} ->
                 e {E.playerVelocity = V.v3 vx PL.jumpAcceleration vz, E.playerOnBottom = False}

              _ -> e

      keyCallback GLFW.KeyTab True = sendEvent . mkUpdateAppEvent $ \app ->
         let layerIds   = L.map LY.layerId (LV.layers . AP.currentLevel $ app)
             actLayId   = AP.activeLayerId app
             greaterIds = L.filter (> actLayId) layerIds
             smallerIds = L.filter (< actLayId) layerIds
             newLayId | not . null $ greaterIds = L.head greaterIds
                      | not . null $ smallerIds = L.head smallerIds
                      | otherwise               = actLayId

             in app {AP.activeLayerId = newLayId}


      keyCallback _ _ = return ()

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      updatePlayerVelocity f = sendEvent . mkUpdateEntityEvent $ \e ->
         case e of
              E.Player {E.playerVelocity = v} -> e {E.playerVelocity = f v}
              _                               -> e

      sendEvent e         = EV.handleEventIO e appDataRef
      mkUpdateEntityEvent = EV.MkEntityEvent . EV.UpdateEntity
      mkUpdateAppEvent    = EV.MkAppEvent . EV.UpdateApp
