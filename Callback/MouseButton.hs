
module Callback.MouseButton where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Control.Monad (when, liftM2)
import qualified Data.IORef as R
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import Gamgine.Control (ifJust, applyIf, applyIfM)
import Gamgine.Utils as GU
import Gamgine.Control ((?))
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.IORef as GR
import qualified AppData as AP
import qualified Entity.Position as EP
import qualified Entity.Bound as EB
import qualified Entity.Id as EI
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Platform as PF
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified Utils as LU
import qualified Updater as UP
import qualified States.MouseInfo as MI
import qualified States.InputInfo as II
import qualified Callback.Common as CC
IMPORT_LENS

type Pressed             = Bool
type MouseButtonCallback = (GLFW.MouseButton -> Pressed -> IO ())

newMouseButtonCallback :: AP.AppDataRef -> AP.AppMode -> MouseButtonCallback
newMouseButtonCallback _ AP.GameMode = \_ _ -> return ()

newMouseButtonCallback appDataRef AP.EditMode = callback_
   where
      callback_ button pressed = do
         mpos <- CC.mousePosition appDataRef
         mods <- CC.pressedModifiers
         let mouseInfo = MI.MouseInfo button (pressed ? II.Pressed $ II.Released) mpos mods
         R.modifyIORef appDataRef (AP.handleMouseEvent mouseInfo)

      callback _ _ = return ()


      resizePlatform = do
         mousePos <- mousePosition
         entity   <- LV.findEntityAt mousePos <$> getL AP.currentLevelL
         ifJust entity $ applyIfM E.isPlatform $ \platform -> do
            let finished = not <$> GLFW.mouseButtonIsPressed GLFW.MouseButton0
                basePos  = B.maxPt . E.platformBound $ platform
            addUpdater $ resizing (mousePos, basePos, finished, E.platformId platform)
            where
               resizing args@(startPos, basePos, finished, id) app = do
                  mousePos <- LU.mousePosInWorldCoords app
                  let diffVec = mousePos - startPos
                      app'    = LE.modL AP.currentLevelL (E.eMap $ ifEntityId id $ \e ->
                                   let bound' = (E.platformBound e) {B.maxPt = basePos + diffVec}
                                       in e {E.platformBound = bound'}) app
                  fin <- finished
                  return $ UP.contineUpdater (app', fin) $ resizing args

      mousePosition = do
         app <- R.readIORef appDataRef
         LU.mousePosInWorldCoords app

      addUpdater updater   = modL AP.updatersL $ (UP.mkUpdater updater :)

      anyCtrlKeyPressed  = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftCtrl)  (GLFW.keyIsPressed GLFW.KeyRightCtrl)
      anyShiftKeyPressed = liftM2 (||) (GLFW.keyIsPressed GLFW.KeyLeftShift) (GLFW.keyIsPressed GLFW.KeyRightShift)

      modL                 = GR.modL appDataRef
      getL                 = GR.getL appDataRef

      ifEntityId id        = applyIf ((== id) . EI.entityId)
