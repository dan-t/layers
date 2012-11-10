
module States.InputInfo where
import qualified Gamgine.Math.Vect as V

-- | mouse position in world coordinates
type MousePos = V.Vect

-- | if the key/mouse button was pressed or released
data InputState = Pressed | Released
