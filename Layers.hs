
import Data.IORef (newIORef, readIORef)
import Data.StateVar (($=))
import System.Exit (exitSuccess)
import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST
import Control.Monad (forM_, mapM_)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Engine as EG
import qualified Gamgine.Ressources as RS
import qualified Gamgine.Gfx as G
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified Background as BG
import qualified AppData as AP
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified GameData.Entity as E
import qualified Entity.Render as ER


winWidth :: Int
winWidth = 1000

winHeight :: Int
winHeight = 1000

ticksPerSecond :: Double
ticksPerSecond = 120

skipTicks :: Double
skipTicks = 1 / ticksPerSecond

maxFrameSkip :: Int
maxFrameSkip = 10

orthoScale :: Double
orthoScale = 45

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
   AP.runApp (gameLoop time) appDataRef
   return ()


gameLoop :: Double -> AP.AppST ()
gameLoop nextFrame = do
   (nextFrame', interpolate) <- updateLoop nextFrame
   io $ do
      GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

   render interpolate
   io GLFW.swapBuffers
   gameLoop nextFrame'


update :: AP.AppST ()
update = return ()


render :: Double -> AP.AppST ()
render interpolate = do
   appDataRef <- ST.get
   io $ do
      appData <- readIORef appDataRef
      BG.render $ AP.background appData
      let currLevelId = AP.currentLevelId appData
          actLayerId  = AP.activeLayerId appData
          rstate = ER.RenderState interpolate $ AP.renderRessources appData

      forM_ (GD.levels $ AP.gameData appData) (\level ->
         if LV.levelId level == currLevelId
            then do mapM_ (ER.render E.LevelScope rstate) $ LV.entities level
                    forM_ (LV.layers level) (\layer ->
                       if LY.layerId layer == actLayerId
                          then mapM_ (ER.render E.ActiveLayerScope rstate) $ LY.entities layer
                          else mapM_ (ER.render E.InactiveLayerScope rstate) $ LY.entities layer)
            else return ())


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
   where
      exitGame = GLFW.closeWindow >> GLFW.terminate >> exitSuccess

      resize width height = do
	 appData <- readIORef appDataRef
         appDataRef $= appData {AP.windowSize = (width, height)}
         updateFrustum
         updateCamera

      updateFrustum = do
	 ad@AP.AppData {AP.windowSize = (width, height)} <- readIORef appDataRef
         let top   = orthoScale * (fromIntegral width / fromIntegral height)
             right = orthoScale

         appDataRef $= ad {AP.frustumSize = (right, top)} 

      updateCamera = do
         AP.AppData {AP.windowSize = (w, h), AP.frustumSize = (r, t)} <- readIORef appDataRef
	 GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
	 GL.glOrtho 0 (G.floatToFloat r) 0 (G.floatToFloat t) (-1) 1


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: AP.AppDataRef -> IO ()
initRessources appDataRef = do
   appData <- readIORef appDataRef
   bg      <- BG.newBackground
   res     <- ER.newRessources
   appDataRef $= appData {AP.background = bg, AP.renderRessources = res}
