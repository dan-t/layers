
module Callback.MouseMove where
import qualified Data.IORef as R
import Control.Applicative ((<$>))
import qualified Utils as LU
import qualified Graphics.UI.GLFW as GLFW
import qualified AppData as AP

type X                 = Int
type Y                 = Int
type MouseMoveCallback = (X -> Y -> IO ())

newMouseMoveCallback :: AP.AppDataRef -> MouseMoveCallback
newMouseMoveCallback appRef = callback
   where
      callback x y = do
         mp <- LU.windowToLevelCoords (x, y) <$> R.readIORef appRef
         R.modifyIORef appRef (AP.handleMouseMoved mp)
