
module Utils where
#include "Gamgine/Utils.cpp"
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Gamgine.Math.Matrix as M
import qualified AppData as AP
import qualified Boundary as BD
import qualified GameData.Level as LV
import qualified GameData.Entity as E
IMPORT_LENS


interpolateFrame :: Double -> V.Vect -> V.Vect -> V.Vect
interpolateFrame nextFrameFraction position velocity =
   position + (velocity * (V.v3 nextFrameFraction nextFrameFraction nextFrameFraction))


levelScrolling :: Double -> AP.AppData -> V.Vect
levelScrolling nextFrameFraction appData =
   let fvec      = V.v3 (fx / 2) (fy / 2) 0
       scroll    = V.map (max 0) (playerPos - fvec)
       maxScroll = V.v3 (ax - fx) (ay - fy) 0
       in (V.minVec scroll maxScroll) * (-1)
   where
      playerPos   = interpolatePos nextFrameFraction $ LE.getL (LV.playerL . AP.currentLevelL) appData
      (ax:.ay:._) = BD.boundaryArea . AP.boundary $ appData
      (fx, fy)    = AP.frustumSize appData

      interpolatePos factor player =
         interpolateFrame factor (E.playerPosition player) (E.playerVelocity player)


mousePosInWorldCoords :: AP.AppData -> IO V.Vect
mousePosInWorldCoords appData = do
   mp <- mousePosInScreenCoords appData
   return $ V.setElem 2 0 $ mp - (levelScrolling 0 appData)


mousePosInScreenCoords :: AP.AppData -> IO V.Vect
mousePosInScreenCoords appData = do
   (x, y) <- GLFW.getMousePosition
   (w, h) <- GLFW.getWindowDimensions
   let mat    = M.worldToWinMatrix (fromIntegral w) (fromIntegral h) (AP.orthoScale appData * (fromIntegral h / fromIntegral w))
       invMat = M.inverseOrIdentity mat
       vec    = V.fromVect4 (multmv invMat (v4 (fromIntegral x) (fromIntegral y) 0 1))
   return $ V.setElem 2 0 vec
