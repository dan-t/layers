
module Callback.Key where
#include "Utils.cpp"
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


newKeyCallback :: AP.AppDataRef -> GLFW.KeyCallback
newKeyCallback appDataRef = callback
   where
      callback win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = quit win
      callback win GLFW.Key'Q      _ GLFW.KeyState'Pressed _ = quit win

      callback _ GLFW.Key'S   _ GLFW.KeyState'Pressed _ = do
         appMode <- AP.appMode <$> R.readIORef appDataRef
         when (appMode == AP.EditMode) $ do
            (gdata, saveTo) <- (AP.gameData &&& AP.saveLevelsTo) <$> R.readIORef appDataRef
            writeFile saveTo (show . TF.toFileData $ gdata)
            putStrLn $ "layers: Levels data written to file '" ++ saveTo ++ "'"

      callback win key _ keyState _ = do
         mpos <- CC.mousePosition win appDataRef
         mods <- II.pressedModifiers win
         case keyState of
              GLFW.KeyState'Pressed  -> do
                 let keyInfo = KI.KeyInfo key II.Pressed mpos mods
                 R.modifyIORef appDataRef (AP.handleKeyEvent keyInfo)

              GLFW.KeyState'Released -> do
                 let keyInfo = KI.KeyInfo key II.Released mpos mods
                 R.modifyIORef appDataRef (AP.handleKeyEvent keyInfo)

              _                      -> return ()

      quit win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess
