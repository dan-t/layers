
import Data.IORef (newIORef, readIORef)
import Data.StateVar (($=))
import System.Exit (exitSuccess)
import Control.Applicative ((<$>))
import qualified Control.Monad.State as ST
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Engine as EG
import qualified Gamgine.Ressources as RS
import qualified Gamgine.Gfx as G
import qualified FileData.Data2 as FD
import qualified Convert.ToGameData as TGD
import qualified Background as BG
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

   dataRef <- newIORef gameData
   initGLFW dataRef
   initGL
   initRessources dataRef

   time <- GLFW.getTime
   GD.runGame (gameLoop time) dataRef
   return ()


gameLoop :: Double -> GD.DataST ()
gameLoop nextFrame = do
   (nextFrame', interpolate) <- updateLoop nextFrame
   io $ do
      GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

   render interpolate
   io GLFW.swapBuffers
   gameLoop nextFrame'


update :: GD.DataST ()
update = return ()


render :: Double -> GD.DataST ()
render interpolate = do
   dataRef <- ST.get
   io $ do
      gameData <- readIORef dataRef
      BG.render $ GD.background gameData
      let level = GD.currentLevel gameData
          res   = GD.renderRessources gameData

      -- render entites of inactive layers
      mapM_ (\LY.Layer {LY.entities = ents} -> mapM_ (ER.render E.InactiveLayerScope res) ents) $ LV.inactiveLayers level
      -- render entities of active layer
      mapM_ (ER.render E.ActiveLayerScope res) $ LY.entities $ LV.activeLayer level
      -- render entities of level scope
      mapM_ (ER.render E.LevelScope res) $ LV.entities level


initGLFW :: GD.DataRef -> IO ()
initGLFW dataRef = do
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
	 gameData <- readIORef dataRef
         dataRef $= gameData {GD.windowSize = (width, height)}
         updateFrustum
         updateCamera

      updateFrustum = do
	 gd@GD.Data {GD.windowSize = (width, height)} <- readIORef dataRef
         let top   = orthoScale * (fromIntegral width / fromIntegral height)
             right = orthoScale

         dataRef $= gd {GD.frustumSize = (right, top)} 

      updateCamera = do
         GD.Data {GD.windowSize = (w, h), GD.frustumSize = (r, t)} <- readIORef dataRef
	 GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
	 GL.glOrtho 0 (G.floatToFloat r) 0 (G.floatToFloat t) (-1) 1


initGL :: IO ()
initGL = do
   GL.glClearColor 0 0 0 0


initRessources :: GD.DataRef -> IO ()
initRessources dataRef = do
   gameData <- readIORef dataRef
   bg       <- BG.newBackground
   res      <- ER.newRessources
   dataRef $= gameData {GD.background = bg, GD.renderRessources = res}
