
module Callback.MouseButton where
#include "Utils.cpp"
import qualified Data.IORef as R
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Control ((?))
import qualified AppData as AP
import qualified Gamgine.State.MouseInfo as MI
import qualified Gamgine.State.InputInfo as II
import qualified Callback.Common as CC
IMPORT_LENS_AS_LE


newMouseButtonCallback :: AP.AppDataRef -> GLFW.MouseButtonCallback
newMouseButtonCallback appDataRef = callback
   where
      callback win button buttonState _ = do
         mpos <- CC.mousePosition win appDataRef
         mods <- II.pressedModifiers win
         let mouseInfo = MI.MouseInfo button (buttonState == GLFW.MouseButtonState'Pressed ? II.Pressed $ II.Released) mpos mods
         R.modifyIORef appDataRef (AP.handleMouseEvent mouseInfo)
