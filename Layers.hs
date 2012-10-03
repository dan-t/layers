
#include "Gamgine/Utils.cpp"
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.StateVar (($=))
import Data.Maybe (catMaybes)
import qualified Data.List as L
import System.Exit (exitSuccess)
import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST
import Control.Monad (forM_, mapM_)
import Control.Arrow ((&&&))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Engine as EG
import qualified Gamgine.Ressources as RS
import Gamgine.Gfx as G
import qualified Gamgine.Utils as GU
import Gamgine.Math.Vect as V
import Defaults
import qualified Utils as LU
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified Background as BG
import qualified Boundary as BD
import qualified AppData as AP
import qualified KeyCallback as KC
import qualified ResolveIntersection as RI
import qualified Event as EV
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
import qualified Entity.Render as ER
import qualified Entity.Update as EU
import qualified Entity.Intersect as EI


updateLoop = EG.updateLoop skipTicks maxFrameSkip update

io = ST.liftIO


main :: IO ()
main = do
   filePath <- RS.getDataFileName "Ressources/Levels2.hs"
   file     <- readFile filePath
   let fileData = read file :: FD.Data
       gameData = TGD.toGameData fileData

   appDataRef <- newIORef $ AP.newAppData gameData
   initGLFW appDataRef
   initGL
   initRessources appDataRef

   time <- GLFW.getTime
   AP.runAppST (gameLoop time) appDataRef
   return ()


gameLoop :: Double -> AP.AppST ()
gameLoop nextFrame = do
   (nextFrame', nextFrameFraction) <- updateLoop nextFrame
   keepInsideBoundary
   events <- handleIntersections
   mapM_ EV.handleEventST events

   io $ do
      GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

   render nextFrameFraction
   io GLFW.swapBuffers
   gameLoop nextFrame'


update :: AP.AppST ()
update = do
   levelGravity <- AP.readActiveLayer LY.gravity
   AP.modifyCurrentLevel $ updateEntities levelGravity
   where
      updateEntities levelGravity level =
         level {LV.entities = L.map (EU.update $ EU.UpdateState levelGravity) $ LV.entities level,
                LV.layers   = L.map updateLayerEntities $ LV.layers level}

      updateLayerEntities layer@LY.Layer {LY.gravity = g} =
         layer {LY.entities = L.map (EU.update $ EU.UpdateState g) $ LY.entities layer}


keepInsideBoundary :: AP.AppST ()
keepInsideBoundary = do
   boundary <- AP.readAppST AP.boundary
   AP.modifyCurrentLevel $ E.eMap (`BD.keepInside` boundary)


handleIntersections :: AP.AppST [EV.Event]
handleIntersections = do
   (curLevel, (actLayer, inactLayers)) <- AP.readAppST $ AP.currentLevel &&& AP.activeAndInactiveLayers
   let levelEnts       = LV.entities curLevel
       actLayerEnts    = LY.entities actLayer
       inactLayersEnts = L.map LY.entities inactLayers

       events  = L.foldl' (\evs es -> evs ++ handleIntersection es es) [] $ [levelEnts, actLayerEnts] ++ inactLayersEnts

   return $ events ++ handleIntersection levelEnts actLayerEnts
   where
      handleIntersection es1 es2 = events
         where
            events = catMaybes [RI.resolveIntersection $ EI.intersect e1 e2 | e1 <- es1, e2 <- es2]


--computeIntersections :: AP.AppST [EI.Intersection]
--computeIntersections = do
--   (curLevel, (actLayer, inactLayers)) <- AP.readAppST $ AP.currentLevel &&& AP.activeAndInactiveLayers
--   let levelEnts           = LV.entities curLevel
--       actLayerEnts        = LY.entities actLayer
--       inactLayersEnts     = map LY.entities inactLayers
--       levelIsects         = intersect levelEnts levelEnts
--       actLayerIsects      = intersect actLayerEnts actLayerEnts
--       levelActLayerIsects = intersect levelEnts actLayerEnts
--       inactLayersIsects   = L.concat $ L.map (\es -> intersect es es) inactLayersEnts
--
--   return $ catMaybes $ levelIsects ++ actLayerIsects ++ levelActLayerIsects ++ inactLayersIsects
--   where
--      intersect es1 es2 = [e1 `EI.intersect` e2 | e1 <- es1, e2 <- es2]


levelScrolling :: Double -> AP.AppST V.Vect
levelScrolling nextFrameFraction = do
   ((fx, fy), (ax:.ay:._)) <- AP.readAppST $ AP.frustumSize &&& (BD.boundaryArea . AP.boundary)
   playerPos               <- AP.readCurrentLevel $ (interpolatePos nextFrameFraction) . LV.getPlayer
   let fvec      = V.v3 (fx / 2) (fy / 2) 0
       scroll    = V.map (max 0) (playerPos - fvec)
       maxScroll = V.v3 (ax - fx) (ay - fy) 0
   return $ (V.minVec scroll maxScroll) * (-1)
   where
      interpolatePos factor player =
         LU.interpolateFrame factor (E.playerPosition player) (E.playerVelocity player)


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   (renderRes, background)             <- AP.readAppST $ AP.renderRessources &&& AP.background
   (curLevel, (actLayer, inactLayers)) <- AP.readAppST $ AP.currentLevel &&& AP.activeAndInactiveLayers
   scrolling                           <- levelScrolling nextFrameFraction
   let renderState              = ER.RenderState nextFrameFraction renderRes
       renderInactLayerEntities = forM_ inactLayers $ (mapM_ $ ER.render E.InactiveLayerScope renderState) . LY.entities
       renderActLayerEntities   = mapM_ (ER.render E.ActiveLayerScope renderState) $ LY.entities actLayer
       renderLevelEntities      = mapM_ (ER.render E.LevelScope renderState) $ LV.entities curLevel
   io $ do
      GL.glTranslatef <<< scrolling
      BG.render background
      renderInactLayerEntities
      renderActLayerEntities
      renderLevelEntities


initGLFW :: AP.AppDataRef -> IO ()
initGLFW appDataRef = do
   GLFW.initialize
   GLFW.openWindow GLFW.defaultDisplayOptions {
      GLFW.displayOptions_width             = winWidth,
      GLFW.displayOptions_height            = winHeight,
      GLFW.displayOptions_windowIsResizable = True
      }

   GLFW.setWindowBufferSwapInterval 1
   GLFW.setWindowSizeCallback resize
   GLFW.setWindowCloseCallback exitGame
   GLFW.setKeyCallback $ KC.newKeyCallback appDataRef
   where
      exitGame = GLFW.closeWindow >> GLFW.terminate >> exitSuccess

      resize width height = do
         modifyAppData (\appData -> appData {AP.windowSize = (width, height)})
         updateFrustum
         updateCamera

      updateFrustum = do
         modifyAppData (\appData ->
            let (width, height) = AP.windowSize appData
                top             = orthoScale * (fromIntegral height / fromIntegral width)
                right           = orthoScale
                in appData {AP.frustumSize = (right, top)})

      updateCamera = do
         AP.AppData {AP.windowSize = (w, h), AP.frustumSize = (r, t)} <- readAppData
	 GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
	 GL.glOrtho 0 (G.floatToFloat r) 0 (G.floatToFloat t) (-1) 1

      modifyAppData f = modifyIORef appDataRef f
      readAppData     = readIORef appDataRef


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: AP.AppDataRef -> IO ()
initRessources appDataRef = do
   appData <- readIORef appDataRef
   bg      <- BG.newBackground
   res     <- ER.newRessources
   appDataRef $= appData {AP.background = bg, AP.renderRessources = res}
