
module Gamgine.Keyboard where
import Graphics.UI.GLFW
import Gamgine.EngineData
import Data.Foldable

handleKeyPress :: (Key, Pressed)
                  -> [(Key, (Pressed -> appData -> IO appData))]
                  -> appData
                  -> IO appData

handleKeyPress (key, pressed) callbacks appData = do
   let normKey = normalizeKey key
   appData' <- foldrM (\(k, kc) appData -> do
      if k == normKey
         then kc pressed appData
         else return appData) appData callbacks

   return appData'

   where
      normalizeKey :: Key -> Key
      normalizeKey k
         | k == CharKey ' ' = KeySpace
         | otherwise        = k
