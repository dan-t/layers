
#include "Gamgine/Utils.cpp"
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.StateVar (($=))
import Data.Maybe (catMaybes)
import Data.Foldable (foldrM)
import qualified Data.List as L
import System.Exit (exitSuccess)
import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST
import Control.Monad (forM_, mapM_)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Engine as EG
import qualified Gamgine.Ressources as RS
import qualified Gamgine.Coroutine as CO
import Gamgine.Gfx as G
import qualified Gamgine.Utils as GU
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import Defaults
import qualified Utils as LU
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified Background as BG
import qualified Boundary as BD
import qualified AppData as AP
import qualified Editor as ED
import qualified KeyCallback as KC
import qualified MouseButtonCallback as MC
import qualified ResolveIntersection as RI
import qualified Event as EV
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
import qualified Rendering.Ressources as RR
import qualified Rendering.Renderer as RD
import qualified Entity.Render as ER
import qualified Entity.Update as EU
import qualified Entity.Intersect as EI
IMPORT_LENS


updateLoop = EG.updateLoop skipTicks maxFrameSkip update

io = ST.liftIO


main :: IO ()
main = do
   filePath <- RS.getDataFileName "Ressources/Levels2.hs"
   file     <- readFile filePath
   let fileData = read file :: FD.Data
       gameData = TGD.toGameData fileData

   appDataRef <- newIORef $ AP.newAppData gameData
   initGLFW appDataRef AP.EditMode
   initGL
   initRessources appDataRef

   time <- GLFW.getTime
   AP.runAppST (gameLoop time) appDataRef
   return ()


gameLoop :: Double -> AP.AppST ()
gameLoop nextFrame = do
   (nextFrame', nextFrameFraction) <- updateLoop nextFrame

   clearGLState
   render nextFrameFraction
   gameLoop nextFrame'


update :: AP.AppST ()
update = do
   levelGravity <- AP.getL (LY.gravityL . AP.activeLayerL)
   AP.modL AP.currentLevelL $ updateEntities levelGravity
   keepInsideBoundary
   events <- handleIntersections
   mapM_ EV.handleEventST events
   where
      updateEntities levelGravity level =
         level {LV.entities = L.map (EU.update $ EU.UpdateState levelGravity) $ LV.entities level,
                LV.layers   = L.map updateLayerEntities $ LV.layers level}

      updateLayerEntities layer@LY.Layer {LY.gravity = g} =
         layer {LY.entities = L.map (EU.update $ EU.UpdateState g) $ LY.entities layer}


keepInsideBoundary :: AP.AppST ()
keepInsideBoundary = do
   boundary <- AP.gets AP.boundary
   AP.modL AP.currentLevelL $ E.eMap (`BD.keepInside` boundary)


handleIntersections :: AP.AppST [EV.Event]
handleIntersections = do
   curLevel                <- AP.getL AP.currentLevelL
   (actLayer, inactLayers) <- AP.gets AP.activeAndInactiveLayers
   let levelEnts       = LV.entities curLevel
       actLayerEnts    = LY.entities actLayer
       inactLayersEnts = L.map LY.entities inactLayers

       events  = L.foldl' (\evs es -> evs ++ handleIntersection es es) [] $ [levelEnts, actLayerEnts] ++ inactLayersEnts

   return $ events ++ handleIntersection levelEnts actLayerEnts
   where
      handleIntersection es1 es2 = events
         where
            events = catMaybes [RI.resolveIntersection $ EI.intersect e1 e2 | e1 <- es1, e2 <- es2]


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   renderRes               <- AP.getL AP.renderRessourcesL
   background              <- AP.getL AP.backgroundL
   curLevel                <- AP.getL AP.currentLevelL
   (actLayer, inactLayers) <- AP.gets AP.activeAndInactiveLayers
   scrolling               <- AP.gets $ LU.levelScrolling nextFrameFraction
   let renderState              = RR.RenderState nextFrameFraction renderRes
       renderInactLayerEntities = forM_ inactLayers $ (mapM_ $ ER.render E.InactiveLayerScope renderState) . LY.entities
       renderActLayerEntities   = mapM_ (ER.render E.ActiveLayerScope renderState) $ LY.entities actLayer
       renderLevelEntities      = mapM_ (ER.render E.LevelScope renderState) $ LV.entities curLevel

   io $ do
      GL.glTranslatef <<< scrolling
      BG.render background
      renderInactLayerEntities
      renderActLayerEntities
      renderLevelEntities

   runRenderers renderState
   renderEditor renderState
   io GLFW.swapBuffers


runRenderers :: RR.RenderState -> AP.AppST ()
runRenderers renderState = do
   rs  <- AP.gets AP.renderers
   rs' <- foldrM (\r rs -> io $ do
                    (finished, r') <- RD.runRenderer r renderState
                    if finished
                       then return rs
                       else return $ r' : rs) [] rs

   AP.setL AP.renderersL rs'


renderEditor :: RR.RenderState -> AP.AppST ()
renderEditor renderState = do
   app      <- AP.get
   mousePos <- io $ LU.mousePosInWorldCoords app
   let editing  = ED.editing . AP.editor $ app
   io $ case editing of
             ED.CreatePlatform pt -> do
                G.withPolyMode GL.gl_LINE $ do
                   GL.glLineWidth 2
                   GL.glColor3f <<<* (0,0,0)
                   let box = (B.Box (V.minVec pt mousePos) (V.maxVec pt mousePos))
                   G.drawBox box

             ED.DefineMovingPath path _ -> do
                GL.glLineWidth 2
                GL.glColor3f <<<* (0,0,0)
                G.draw GL.gl_LINE_STRIP (mousePos : path)

             otherwise -> return ()


clearGLState :: AP.AppST ()
clearGLState = io $ do
   GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
   GL.glMatrixMode GL.gl_MODELVIEW
   GL.glLoadIdentity


initGLFW :: AP.AppDataRef -> AP.AppMode -> IO ()
initGLFW appDataRef appMode = do
   GLFW.initialize
   GLFW.openWindow GLFW.defaultDisplayOptions {
      GLFW.displayOptions_width             = winWidth,
      GLFW.displayOptions_height            = winHeight,
      GLFW.displayOptions_windowIsResizable = True
      }

   GLFW.setWindowBufferSwapInterval 1
   GLFW.setWindowSizeCallback resize
   GLFW.setWindowCloseCallback exitGame
   GLFW.setKeyCallback $ KC.newKeyCallback appDataRef appMode
   GLFW.setMouseButtonCallback $ MC.newMouseButtonCallback appDataRef appMode
   where
      exitGame = GLFW.closeWindow >> GLFW.terminate >> exitSuccess

      resize width height = do
         setL AP.windowSizeL (width, height)
         updateFrustum
         updateCamera

      updateFrustum = do
         modApp (\app ->
            let (width, height) = AP.windowSize app
                orthoScale      = LU.orthoScale app
                top             = orthoScale * (fromIntegral height / fromIntegral width)
                right           = orthoScale
                in app {AP.frustumSize = (right, top)})

      updateCamera = do
         (w, h) <- getL AP.windowSizeL
         (r, t) <- getL AP.frustumSizeL
	 GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
	 GL.glOrtho 0 (G.floatToFloat r) 0 (G.floatToFloat t) (-1) 1

      getL lens       = GU.mapIORef (LE.getL lens) appDataRef
      setL lens value = modifyIORef appDataRef $ LE.setL lens value
      modApp          = modifyIORef appDataRef


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: AP.AppDataRef -> IO ()
initRessources appDataRef = do
   bg  <- BG.newBackground
   res <- RR.newRessources
   modifyIORef appDataRef $ \app -> app {AP.background = bg, AP.renderRessources = res}
