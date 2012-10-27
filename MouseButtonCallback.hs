
module MouseButtonCallback where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.IORef as R
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.IORef as GR
import qualified AppData as AP
import qualified Editor as ED
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
         ctrlPressed  <- isAnyKeyPressed ctrlKeys
         shiftPressed <- isAnyKeyPressed shiftKeys
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
               moving (startPos, basePos, finished, id) app = do
                  mousePos <- LU.mousePosInWorldCoords app
                  let newBasePos = basePos + (mousePos - startPos)
                      app'       = LE.modL AP.currentLevelL (E.eMap $ \e ->
                                      if id /= EI.entityId e
                                         then e
                                         else EP.setCurrentPosition e newBasePos) app
                  fin <- finished
                  return $ UP.contineUpdater (app', fin) $ moving (startPos, basePos, finished, id)


      resizePlatform = return ()

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
            resizing (startPos, finished, id) app = do
               mousePos <- LU.mousePosInWorldCoords app
               let app' = LE.modL AP.activeLayerL (E.eMap $ \e ->
                             if id /= EI.entityId e
                                then e
                                else let newMinPt = V.minVec startPos mousePos
                                         newMaxPt = V.maxVec startPos mousePos
                                         diffVec  = V.map abs $ newMaxPt - newMinPt
                                         in e {E.platformPosition = Left newMinPt,
                                               E.platformBound    = B.Box (V.v3 0 0 0) diffVec}) app
               fin <- finished
               return $ UP.contineUpdater (app', fin) $ resizing (startPos, finished, id)


      mousePosition = do
         app <- readApp
         LU.mousePosInWorldCoords app

      addUpdater updater   = modL AP.updatersL $ (UP.mkUpdater updater :)

      isAnyKeyPressed keys = L.any (== True) <$> mapM GLFW.keyIsPressed keys
      shiftKeys            = [GLFW.KeyLeftShift, GLFW.KeyRightShift]
      ctrlKeys             = [GLFW.KeyLeftCtrl, GLFW.KeyRightCtrl]

      readApp              = R.readIORef appDataRef
      modifyApp            = R.modifyIORef appDataRef
      mapApp               = GR.mapIORef appDataRef

      modL                 = GR.modL appDataRef
      getL                 = GR.getL appDataRef

      just (Just e) f      = f e
      just _        _      = return ()
