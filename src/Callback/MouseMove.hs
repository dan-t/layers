
module Callback.MouseMove where
import qualified Data.IORef as R
import Control.Applicative ((<$>))
import qualified Utils as LU
import qualified Graphics.UI.GLFW as GLFW
import qualified AppData as AP


newMouseMoveCallback :: AP.AppDataRef -> GLFW.CursorPosCallback
newMouseMoveCallback appRef = callback
   where
      callback win x y = do
         mp <- LU.windowToLevelCoords (floor x, floor y) <$> R.readIORef appRef
         R.modifyIORef appRef (AP.handleMouseMoved mp)
