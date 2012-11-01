
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
import qualified Gamgine.IORef as GR
import Gamgine.Gfx as G
import qualified Gamgine.Utils as GU
import Defaults as DF
import qualified Utils as U
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified AppData as AP
import qualified Callback.Key as KC
import qualified Callback.MouseButton as MC
import qualified Event as EV
import qualified GameData.Entity as E
import qualified Rendering.Ressources as RR
import qualified Rendering.Renderer as RD
import qualified Updater as UP
import qualified Level.Update as LU
import qualified Level.Render as LR
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
   runUpdaters
   (events, level') <- LU.update <$> GR.getsL AP.currentLevelL
   GR.putL AP.currentLevelL level'
   mapM_ EV.handleEventST events


runUpdaters :: AP.AppST ()
runUpdaters = do
   app          <- GR.get
   ups          <- GR.gets AP.updaters
   (app', ups') <- foldrM (\up (app, ups) -> io $ do
      ((app', finished), up') <- UP.runUpdater up app
      if finished
         then return (app', ups)
         else return $ (app', up' : ups)) (app, []) ups

   GR.put $ app' {AP.updaters = ups'}


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   renderRes <- GR.getsL AP.renderRessourcesL
   currLevel <- GR.getsL AP.currentLevelL
   scrolling <- GR.gets $ U.levelScrolling nextFrameFraction
   let renderState = RR.RenderState nextFrameFraction renderRes
   io $ do
      GL.glTranslatef <<< scrolling
      LR.render renderState currLevel

   runRenderers renderState
   io GLFW.swapBuffers


runRenderers :: RR.RenderState -> AP.AppST ()
runRenderers renderState = do
   rs  <- GR.gets AP.renderers
   rs' <- foldrM (\r rs -> io $ do
                    (finished, r') <- RD.runRenderer r renderState
                    if finished
                       then return rs
                       else return $ r' : rs) [] rs

   GR.putL AP.renderersL rs'


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
   GLFW.setMouseWheelCallback $ if appMode == AP.EditMode then updateOrthoScale else \_ -> return ()
   where
      exitGame = GLFW.closeWindow >> GLFW.terminate >> exitSuccess

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
