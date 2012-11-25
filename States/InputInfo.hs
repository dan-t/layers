
module States.InputInfo where
import qualified Gamgine.Math.Vect as V

data Modifier = Ctrl | Alt | Shift deriving (Eq, Ord)

-- | mouse position in world coordinates
type MousePos = V.Vect

-- | if the key/mouse button was pressed or released
data InputState = Pressed | Released deriving (Eq, Ord)
