
module Callback.MouseMove where
import qualified Data.IORef as R
import qualified Utils as LU
import qualified Graphics.UI.GLFW as GLFW
import qualified AppData as AP

type X                 = Int
type Y                 = Int
type MouseMoveCallback = (X -> Y -> IO ())

newMouseMoveCallback :: AP.AppDataRef -> AP.AppMode -> MouseMoveCallback
newMouseMoveCallback appRef _ = callback
   where
      callback x y = do
         mp <- R.readIORef appRef >>= LU.windowToWorldCoords (x, y)
         R.modifyIORef appRef (AP.handleMouseMoved mp)
