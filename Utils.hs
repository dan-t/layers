
module Utils where
#include "Gamgine/Utils.cpp"
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative ((<$>))
import Gamgine.Math.Vect as V
import qualified Gamgine.Math.Matrix as M
import qualified Gamgine.Math.Box as B
import qualified AppData as AP
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Boundary as BD
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
      (ax:.ay:._) = BD.boundaryArea $ LE.getL (LV.boundaryL . AP.currentLevelL) appData
      (fx, fy)    = AP.frustumSize appData

      interpolatePos factor player =
         interpolateFrame factor (E.playerPosition player) (E.playerVelocity player)


mousePosInLevelCoords :: AP.AppData -> IO V.Vect
mousePosInLevelCoords appData = do
   xy <- GLFW.getMousePosition
   return $ windowToLevelCoords xy appData


windowToLevelCoords :: (Int, Int) -> AP.AppData -> V.Vect
windowToLevelCoords xy appData =
   V.setElem 2 0 $ (windowToWorldCoords xy appData) - (levelScrolling 0 appData)


windowToWorldCoords :: (Int, Int) -> AP.AppData -> V.Vect
windowToWorldCoords (x, y) appData =
   let (w, h)     = AP.windowSize appData
       mat        = M.worldToWinMatrix (fromIntegral w) (fromIntegral h) (AP.orthoScale appData * (fromIntegral h / fromIntegral w))
       invMat     = M.inverseOrIdentity mat
       worldCoord = V.fromVect4 (multmv invMat (v4 (fromIntegral x) (fromIntegral y) 0 1))
       in worldCoord


-- | ensure that the boundary size of all levels is at least of the size of the window
updateBoundarySize :: AP.AppData -> AP.AppData
updateBoundarySize app@AP.AppData {AP.windowSize = (width, height)} =
   let wcoord     = windowToWorldCoords (width, -height) app
       boundMaxPt = B.maxPtL . BD.boxL . LV.boundaryL
       in LE.modL (GD.levelsL . AP.gameDataL) (LE.modL boundMaxPt (V.maxVec wcoord) <$>) app
