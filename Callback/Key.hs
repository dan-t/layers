
module Callback.Key where
#include "Gamgine/Utils.cpp"
import qualified Data.IORef as R
import System.Exit (exitSuccess)
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Control ((?))
import qualified AppData as AP
import qualified States.KeyInfo as KI
import qualified Callback.Common as CC
IMPORT_LENS

type Pressed     = Bool
type KeyCallback = (GLFW.Key -> Pressed -> IO ())

newKeyCallback :: AP.AppDataRef -> KeyCallback
newKeyCallback appDataRef = callback
   where
      callback GLFW.KeyEsc        True = quit
      callback (GLFW.CharKey 'Q') True = quit
      callback key pressed             = do
         mpos <- CC.mousePosition appDataRef
         mods <- CC.pressedModifiers
         let keyInfo = KI.KeyInfo key (pressed ? KI.Pressed $ KI.Released) mpos mods
         R.modifyIORef appDataRef (AP.handleKeyEvent keyInfo)

      quit = GLFW.closeWindow >> GLFW.terminate >> exitSuccess
