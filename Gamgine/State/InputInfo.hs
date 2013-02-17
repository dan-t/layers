
module Gamgine.State.InputInfo where
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (liftM2)
import Gamgine.Control ((?))
import qualified Gamgine.Math.Vect as V

data Modifier = Ctrl | Alt | Shift deriving (Eq, Ord)

-- | mouse position in world coordinates
type MousePos = V.Vect

-- | if the key/mouse button was pressed or released
data InputState = Pressed | Released deriving (Eq, Ord)

pressedModifiers :: IO [Modifier]
pressedModifiers = do
   ctrl  <- isCtrlPressed
   shift <- isShiftPressed
   alt   <- isAltPressed
   return $ (ctrl ? [Ctrl] $ []) ++ (shift ? [Shift] $ []) ++ (alt ? [Alt] $ [])
         
isCtrlPressed, isAltPressed, isShiftPressed :: IO Bool
isCtrlPressed  = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftCtrl)  (GLFW.keyIsPressed GLFW.KeyRightCtrl)
isAltPressed   = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftAlt)   (GLFW.keyIsPressed GLFW.KeyRightAlt)
isShiftPressed = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftShift) (GLFW.keyIsPressed GLFW.KeyRightShift)
