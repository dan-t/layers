
module Callback.Common where
import qualified Data.IORef as R
import qualified Utils as LU

mousePosition appRef = R.readIORef appRef >>= LU.mousePosInLevelCoords
