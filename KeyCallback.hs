
module KeyCallback where

import Data.IORef (modifyIORef)
import qualified Data.Lens.Strict as LE
import qualified Graphics.UI.GLFW as GLFW
import qualified Event as EV
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified Entity.HandleEvent as HE


newKeyCallback :: AP.AppDataRef -> (GLFW.Key -> Bool -> IO ())
newKeyCallback appDataRef = keyCallback
   where
      keyCallback GLFW.KeyLeft True = sendEvent $ EV.PlayerStartsMoving E.ToTheLeft

      keyCallback GLFW.KeyLeft False = sendEvent $ EV.PlayerStopsMoving E.ToTheLeft

      keyCallback GLFW.KeyRight True = sendEvent $ EV.PlayerStartsMoving E.ToTheRight

      keyCallback GLFW.KeyRight False = sendEvent $ EV.PlayerStopsMoving E.ToTheRight

      keyCallback _ _ = return ()

      sendEvent e          = modifyCurrentLevel $ E.eMap (HE.handleEvent e)
      modifyCurrentLevel f = modifyIORef appDataRef $ LE.modL AP.currentLevelLens f
