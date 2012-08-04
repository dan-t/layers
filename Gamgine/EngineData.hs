
module Gamgine.EngineData where
import Graphics.UI.GLFW

type Pressed = Bool

data EngineData appData = EngineData {
   keyCallbacks         :: [(Key        , (Pressed -> appData -> IO appData))],
   mouseButtonCallbacks :: [(MouseButton, (Pressed -> appData -> IO appData))]
   }
