
module Callback.Common where
import qualified Data.IORef as R
import qualified Utils as LU

mousePosition win appRef = R.readIORef appRef >>= LU.mousePosInLevelCoords win
