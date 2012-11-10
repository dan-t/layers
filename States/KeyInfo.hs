
module States.KeyInfo (module II, KeyInfo(..)) where
import qualified Graphics.UI.GLFW as GLFW
import qualified Gamgine.Math.Vect as V
import States.InputInfo as II

-- | info for the pressed/released key
data KeyInfo = KeyInfo {
   -- the pressed/released key
   key       :: GLFW.Key,
   -- if the key was pressed/released
   status    :: II.InputState,
   -- the current mouse position in world coordinates
   mousePos  :: II.MousePos,
   -- if a control key is additionally pressed
   withCtrl  :: Bool,
   -- if a shift key is additionally pressed
   withShift :: Bool,
   -- if the alt key is additionally pressed
   withAlt   :: Bool
   }
