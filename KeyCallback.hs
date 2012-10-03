
module KeyCallback where

import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Event as EV
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL


newKeyCallback :: AP.AppDataRef -> (GLFW.Key -> Bool -> IO ())
newKeyCallback appDataRef = keyCallback
   where
      keyCallback GLFW.KeyLeft True = accelerateToTheLeft

      keyCallback GLFW.KeyLeft False = accelerateToTheRight

      keyCallback GLFW.KeyRight True = accelerateToTheRight

      keyCallback GLFW.KeyRight False = accelerateToTheLeft

      keyCallback GLFW.KeyUp True = sendEvent $ Right $ EV.UpdateEntity $ \e ->
         case e of
              E.Player {E.playerVelocity = vx:.vy:.vz:.(), E.playerOnBottom = True} ->
                 e {E.playerVelocity = V.v3 vx PL.jumpAcceleration vz, E.playerOnBottom = False}

              _ -> e


      keyCallback _ _ = return ()

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      updatePlayerVelocity f = sendEvent $ Right $ EV.UpdateEntity $ \e ->
         case e of
              E.Player {E.playerVelocity = v} -> e {E.playerVelocity = f v}
              _                               -> e

      sendEvent e = EV.handleEventIO e appDataRef
