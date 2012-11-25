
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
   -- if a modifier key is additionally pressed
   modifiers :: [II.Modifier]
   }
