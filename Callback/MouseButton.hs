
module Callback.MouseButton where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Control.Monad (when, liftM2)
import qualified Data.IORef as R
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import Gamgine.Utils as GU
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
IMPORT_LENS

type Pressed             = Bool
type MouseButtonCallback = (GLFW.MouseButton -> Pressed -> IO ())

newMouseButtonCallback :: AP.AppDataRef -> AP.AppMode -> MouseButtonCallback
newMouseButtonCallback _ AP.GameMode = \_ _ -> return ()

newMouseButtonCallback appDataRef AP.EditMode = callback
   where
      callback GLFW.MouseButton0 True = do
         ctrlPressed  <- anyCtrlKeyPressed
         shiftPressed <- anyShiftKeyPressed
         when ctrlPressed  moveEntity
         when shiftPressed resizePlatform
         when (not ctrlPressed && not shiftPressed) createPlatform

      callback _ _ = return ()

      moveEntity = do
         mousePos <- mousePosition
         entity   <- LV.findEntityAt mousePos <$> getL AP.currentLevelL
         just entity $ \e -> do
            let finished = not <$> GLFW.mouseButtonIsPressed GLFW.MouseButton0
            addUpdater $ moving (mousePos, EP.currentPosition e, finished, EI.entityId e)
            where
               moving args@(startPos, basePos, finished, id) app = do
                  mousePos <- LU.mousePosInWorldCoords app
                  let newBasePos = basePos + (mousePos - startPos)
                      app'       = LE.modL AP.currentLevelL (E.eMap $ ifEntityId id $ \e ->
                                      EP.setPosition e newBasePos) app
                  fin <- finished
                  return $ UP.contineUpdater (app', fin) $ moving args


      resizePlatform = do
         mousePos <- mousePosition
         entity   <- LV.findEntityAt mousePos <$> getL AP.currentLevelL
         justPlatform entity $ \platform -> do
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


      createPlatform = do
         mousePos   <- mousePosition
         platformId <- LV.freeEntityId <$> getL AP.currentLevelL
         modL AP.currentLevelL $ \level ->
            let bound  = B.Box V.nullVec V.nullVec
                pos    = Left mousePos
                in LV.addEntity (PF.newPlatform platformId pos bound) LV.ToActiveLayer level

         let finished = not <$> GLFW.mouseButtonIsPressed GLFW.MouseButton0
         addUpdater $ resizing (mousePos, finished, platformId)
         where
            resizing args@(startPos, finished, id) app = do
               mousePos <- LU.mousePosInWorldCoords app
               let app' = LE.modL AP.activeLayerL (E.eMap $ ifEntityId id $ \e ->
                             let newMinPt = V.minVec startPos mousePos
                                 newMaxPt = V.maxVec startPos mousePos
                                 diffVec  = V.map abs $ newMaxPt - newMinPt
                                 in e {E.platformPosition = Left newMinPt,
                                       E.platformBound    = B.Box (V.v3 0 0 0) diffVec}) app
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

      ifEntityId id        = GU.applyIf ((== id) . EI.entityId)

      just (Just e) f      = f e
      just _        _      = return ()

      justPlatform (Just p@E.Platform {}) f = f p
      justPlatform _                      _ = return ()
