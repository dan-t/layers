
module Callback.Common where
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (liftM2)
import Gamgine.Control ((?))
import qualified Data.IORef as R
import qualified Utils as LU
import qualified States.InputInfo as II


mousePosition appRef = R.readIORef appRef >>= LU.mousePosInLevelCoords


pressedModifiers = do
   ctrl  <- isCtrlPressed
   shift <- isShiftPressed
   alt   <- isAltPressed
   return $ (ctrl ? [II.Ctrl] $ []) ++ (shift ? [II.Shift] $ []) ++ (alt ? [II.Alt] $ [])
         

isCtrlPressed  = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftCtrl)  (GLFW.keyIsPressed GLFW.KeyRightCtrl)
isAltPressed   = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftAlt)   (GLFW.keyIsPressed GLFW.KeyRightAlt)
isShiftPressed = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftShift) (GLFW.keyIsPressed GLFW.KeyRightShift)
