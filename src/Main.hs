
module Main where
#include "Utils.cpp"
import Control.Applicative ((<$>))
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.List as L
import System.Exit (exitSuccess)
import qualified Control.Monad.State as ST
import Control.Monad (when, void)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Gamgine.Control ((?))
import qualified Gamgine.Engine as EG
import qualified Gamgine.Lens.IORef as GR
import qualified Ressources as RES
import qualified Gamgine.State.RenderState as RS
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<<))
import qualified Gamgine.Font.GLF as GLF
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
import qualified GameData.Data as GD
IMPORT_LENS_AS_LE


io = ST.liftIO


main :: IO ()
main = do
   args <- LA.getLayersArgs
   file <- readFile . LA.loadLevelsFrom $ args
   let fileData = read file :: FD.Data
       gameData = TGD.toGameData fileData
       editMode = LA.editMode args ? AP.EditMode $ AP.GameMode

   GLFW.init
   GLFW.windowHint $ GLFW.WindowHint'Resizable True
   GLFW.swapInterval 1
   Just win <- GLFW.createWindow winWidth winHeight "" Nothing Nothing
   GLFW.makeContextCurrent (Just win)

   initGL
   GLF.init

   appDataRef <- newIORef $ AP.newAppData win gameData (LA.loadLevelsFrom args) (LA.saveLevelsTo args) editMode
   initCallbacks appDataRef editMode
   initRessources appDataRef

   Just time <- GLFW.getTime
   AP.runAppST (gameLoop time) appDataRef
   return ()


gameLoop :: Double -> AP.AppST ()
gameLoop nextFrame = do
   io GLFW.pollEvents
   (nextFrame', nextFrameFraction) <- updateLoop nextFrame

   clearGLState
   render nextFrameFraction
   win <- GR.gets AP.window
   io $ GLFW.swapBuffers win
   gameLoop nextFrame'
   where
      updateLoop = EG.mkUpdateLoop ticksPerSecond maxFrameSkip update


update :: AP.AppST ()
update =  do
   GR.modify AP.update
   appMode <- GR.gets AP.appMode
   when (appMode == AP.GameMode) $ do
      gdata <- GR.getsL AP.gameDataL
      when (GD.levelFinished gdata) $ do
         GR.modifyL AP.gameDataL GD.toNextLevel


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   gdata <- GR.getsL AP.gameDataL
   if GD.gameFinished gdata
      then renderEndScreen
      else do
         app  <- GR.get
         io $ GL.glTranslatef <<< U.levelScrolling nextFrameFraction app
         app' <- io $ AP.render nextFrameFraction app
         GR.put app'


renderEndScreen :: AP.AppST ()
renderEndScreen = do
   font     <- RR.fontId RR.Crystal <$> GR.gets AP.renderRessources
   (fx, fy) <- GR.gets AP.frustumSize
   io $ do
      GLF.setCurrentFont font
      GLF.Bounds (minx, miny) (maxx, maxy) <- GLF.getStringBounds endMsg
      GL.glTranslatef <<< G.xyz (fx / 2 - ((minx + maxx) * 2)) (fy / 2) 0
      GL.glScalef <<< G.xyz 3.75 3 3
      GLF.drawSolidString endMsg
   where
      endMsg = "END"


clearGLState :: AP.AppST ()
clearGLState = io $ do
   GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
   GL.glMatrixMode GL.gl_MODELVIEW
   GL.glLoadIdentity


initCallbacks :: AP.AppDataRef -> AP.AppMode -> IO ()
initCallbacks appDataRef appMode = do
   win <- getL AP.windowL
   GLFW.setWindowSizeCallback win (Just resize)
   GLFW.setWindowCloseCallback win (Just quit)
   GLFW.setKeyCallback win (Just $ KC.newKeyCallback appDataRef)
   GLFW.setMouseButtonCallback win (Just $ MC.newMouseButtonCallback appDataRef)
   GLFW.setCursorPosCallback win (Just $ MM.newMouseMoveCallback appDataRef)
   GLFW.setScrollCallback win (Just $ if appMode == AP.EditMode then updateOrthoScale else \_ _ _ -> return ())
   where
      resize _ width height = do
         setL AP.windowSizeL (width, height)
         modify U.updateBoundarySize
         updateFrustum
         updateCamera

      quit win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess

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

      updateOrthoScale _ _ yoffset  = do
         modL AP.orthoScaleL (+ yoffset)
         modify U.updateBoundarySize
         updateFrustum
         updateCamera

      getL   = GR.getL appDataRef
      setL   = GR.setL appDataRef
      modL   = GR.modL appDataRef
      read   = readIORef appDataRef
      modify = modifyIORef appDataRef


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: AP.AppDataRef -> IO ()
initRessources appDataRef = do
   res <- RR.newRessources
   modifyIORef appDataRef $ \app -> app {AP.renderRessources = res}
