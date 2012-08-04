
module Gamgine.Mouse where
import Graphics.UI.GLFW
import Gamgine.EngineData
import Data.Foldable


handleMouseButtonPress :: (MouseButton, Pressed)
                          -> [(MouseButton, (Pressed -> appData -> IO appData))]
                          -> appData
                          -> IO appData

handleMouseButtonPress (button, pressed) callbacks appData = do
   appData' <- foldrM (\(mb, bc) appData -> do
      if mb == button
         then bc pressed appData
         else return appData) appData callbacks

   return appData'
