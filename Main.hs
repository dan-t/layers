
module Main where
#include "Gamgine/Utils.cpp"
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.List as L
import System.Exit (exitSuccess)
import qualified Control.Monad.State as ST
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Gamgine.Control ((?))
import qualified Gamgine.Engine as EG
import qualified Gamgine.IORef as GR
import Gamgine.Gfx as G
import Defaults as DF
import qualified Utils as U
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified AppData as AP
import qualified Callback.Key as KC
import qualified Callback.MouseButton as MC
import qualified Callback.MouseMove as MM
import qualified Rendering.Ressources as RR
import qualified LayersArgs as LA
IMPORT_LENS


updateLoop = EG.updateLoop skipTicks maxFrameSkip update

io = ST.liftIO


main :: IO ()
main = do
   args <- LA.getLayersArgs
   file <- readFile . LA.loadLevelsFrom $ args
   let fileData = read file :: FD.Data
       gameData = TGD.toGameData fileData
       editMode = LA.editMode args ? AP.EditMode $ AP.GameMode

   appDataRef <- newIORef $ AP.newAppData gameData (LA.loadLevelsFrom args) (LA.saveLevelsTo args) editMode
   initGLFW appDataRef editMode
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
update = GR.modify AP.update


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   app  <- GR.get
   io $ GL.glTranslatef <<< U.levelScrolling nextFrameFraction app
   app' <- io $ AP.render nextFrameFraction app
   GR.put app'
   io GLFW.swapBuffers


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
   GLFW.setWindowCloseCallback quit
   GLFW.setKeyCallback $ KC.newKeyCallback appDataRef
   GLFW.setMouseButtonCallback $ MC.newMouseButtonCallback appDataRef
   GLFW.setMousePositionCallback $ MM.newMouseMoveCallback appDataRef
   GLFW.setMouseWheelCallback $ if appMode == AP.EditMode then updateOrthoScale else \_ -> return ()
   where
      resize width height = do
         setL AP.windowSizeL (width, height)
         updateFrustum
         updateCamera

      updateFrustum = do
         modify (\app ->
            let (width, height) = AP.windowSize app
                orthoScale      = AP.orthoScale app
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

      updateOrthoScale mouseWheelPos = do
         setL AP.orthoScaleL (DF.orthoScale + fromIntegral mouseWheelPos)
         updateFrustum
         updateCamera

      quit   = GLFW.closeWindow >> GLFW.terminate >> exitSuccess
      getL   = GR.getL appDataRef
      setL   = GR.setL appDataRef
      modify = modifyIORef appDataRef


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: AP.AppDataRef -> IO ()
initRessources appDataRef = do
   res <- RR.newRessources
   modifyIORef appDataRef $ \app -> app {AP.renderRessources = res}
