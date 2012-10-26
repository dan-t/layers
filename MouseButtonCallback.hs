
module MouseButtonCallback where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Data.IORef as R
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.BoxTree as BT
import qualified Event as EV
import qualified AppData as AP
import qualified Editor as ED
import qualified Entity.Position as EP
import qualified Entity.Bound as EB
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Platform as PF
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified Utils as LU
IMPORT_LENS

type Pressed             = Bool
type MouseButtonCallback = (GLFW.MouseButton -> Pressed -> IO ())

newMouseButtonCallback :: AP.AppDataRef -> AP.AppMode -> MouseButtonCallback
newMouseButtonCallback _ AP.GameMode = \_ _ -> return ()

newMouseButtonCallback appDataRef AP.EditMode = callback
   where
      callback GLFW.MouseButton0 True = do
         app          <- readApp
         ctrlPressed  <- isAnyKeyPressed ctrlKeys
         shiftPressed <- isAnyKeyPressed shiftKeys
         mousePos     <- V.setElem 2 0 <$> LU.mousePosInWorldCoords app
         let editing     = ED.editing . AP.editor $ app
             foundEntity = LV.findEntityAt mousePos $ LE.getL AP.currentLevelL app

         case editing of
              ED.NoEdition 
                 | ctrlPressed  -> just foundEntity $ \fe -> updateEntityEvent $ \e ->
                                      if E.entityId e == E.entityId fe
                                         then EP.updateCurrentPosition e mousePos
                                         else e

                 | shiftPressed -> just foundEntity $ \fe -> updateEditing $ 
                                      ED.ResizePlatform (B.maxPt . BT.asBox . EB.bound $ fe)
                                                        mousePos (E.entityId fe)

                 | otherwise    -> updateEditing $ ED.CreatePlatform mousePos

              _ -> return () 

      callback GLFW.MouseButton0 False = do
         app      <- readApp
         mousePos <- V.setElem 2 0 <$> LU.mousePosInWorldCoords app
         let editing = ED.editing . AP.editor $ app
         case editing of
              ED.CreatePlatform pt -> updateAppEvent $ \app ->
                 let freeId = LV.nextFreeEntityId $ LE.getL AP.currentLevelL app
                     bound  = B.Box (V.v3 0 0 0) (V.map abs $ mousePos - pt)
                     pos    = Left $ V.minVec mousePos pt
                     in LE.modL (LY.entitiesL . AP.activeLayerL) (\es -> PF.newPlatform freeId pos bound : es)
                           $ LE.setL (ED.editingL . AP.editorL) ED.NoEdition app

              _ -> return ()


      callback _ _ = return ()

      updateEditing edit   = updateAppEvent $ LE.setL (ED.editingL . AP.editorL) edit

      isAnyKeyPressed keys = L.any (== True) <$> mapM GLFW.keyIsPressed keys
      shiftKeys            = [GLFW.KeyLeftShift, GLFW.KeyRightShift]
      ctrlKeys             = [GLFW.KeyLeftCtrl, GLFW.KeyRightCtrl]

      readApp              = R.readIORef appDataRef
      modifyApp            = R.modifyIORef appDataRef

      just (Just e) f      = f e
      just _        _      = return ()

      updateEntityEvent    = sendEvent . mkUpdateEntityEvent
      updateAppEvent       = sendEvent . mkUpdateAppEvent
      mkUpdateEntityEvent  = EV.MkEntityEvent . EV.UpdateEntity
      mkUpdateAppEvent     = EV.MkAppEvent . EV.UpdateApp
      sendEvent e          = EV.handleEventIO e appDataRef
