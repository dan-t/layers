
module Callback.MouseButton where
#include "Gamgine/Utils.cpp"
import qualified Data.IORef as R
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Control ((?))
import qualified AppData as AP
import qualified States.MouseInfo as MI
import qualified States.InputInfo as II
import qualified Callback.Common as CC
IMPORT_LENS

type Pressed             = Bool
type MouseButtonCallback = (GLFW.MouseButton -> Pressed -> IO ())

newMouseButtonCallback :: AP.AppDataRef -> AP.AppMode -> MouseButtonCallback
newMouseButtonCallback _ AP.GameMode = \_ _ -> return ()

newMouseButtonCallback appDataRef AP.EditMode = callback
   where
      callback button pressed = do
         mpos <- CC.mousePosition appDataRef
         mods <- CC.pressedModifiers
         let mouseInfo = MI.MouseInfo button (pressed ? II.Pressed $ II.Released) mpos mods
         R.modifyIORef appDataRef (AP.handleMouseEvent mouseInfo)
