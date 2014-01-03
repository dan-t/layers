
module Callback.Key where
#include "Gamgine/Utils.cpp"
import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.IORef as R
import System.Exit (exitSuccess)
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Control ((?))
import qualified AppData as AP
import qualified Gamgine.State.KeyInfo as KI
import qualified Gamgine.State.InputInfo as II
import qualified Callback.Common as CC
import qualified Convert.ToFileData as TF
import qualified Utils as U
IMPORT_LENS_AS_LE

type Pressed     = Bool
type KeyCallback = (GLFW.Key -> Pressed -> IO ())

newKeyCallback :: AP.AppDataRef -> KeyCallback
newKeyCallback appDataRef = callback
   where
      callback GLFW.KeyEsc        True = quit
      callback (GLFW.CharKey 'Q') True = quit

      callback (GLFW.CharKey 'S') True = do
         appMode <- AP.appMode <$> R.readIORef appDataRef
         when (appMode == AP.EditMode) $ do
            (gdata, saveTo) <- (AP.gameData &&& AP.saveLevelsTo) <$> R.readIORef appDataRef
            writeFile saveTo (show . TF.toFileData $ gdata)
            putStrLn $ "layers: Levels data written to file '" ++ saveTo ++ "'"

      callback key pressed             = do
         mpos <- CC.mousePosition appDataRef
         mods <- II.pressedModifiers
         let keyInfo = KI.KeyInfo key (pressed ? KI.Pressed $ KI.Released) mpos mods
         R.modifyIORef appDataRef (AP.handleKeyEvent keyInfo)

      quit = GLFW.closeWindow >> GLFW.terminate >> exitSuccess
