
#include "Gamgine/Utils.cpp"
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.StateVar (($=))
import qualified Data.Lens.Strict as LE
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
import qualified Entity.Update as EU


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
   (nextFrame', nextFrameFraction) <- updateLoop nextFrame
   io $ do
      GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

   render nextFrameFraction
   io GLFW.swapBuffers
   gameLoop nextFrame'


update :: AP.AppST ()
update = do
   appDataRef <- ST.get
   io $ modifyIORef appDataRef (LE.modL AP.currentLevel updateEntities)
   where
      updateEntities (Just level) = Just $
         level {LV.entities = map EU.update $ LV.entities level,
                LV.layers   = map updateLayerEntities $ LV.layers level}

      updateEntities _ = Nothing

      updateLayerEntities layer =
         layer {LY.entities = map EU.update $ LY.entities layer}


render :: Double -> AP.AppST ()
render nextFrameFraction = do
   appDataRef <- ST.get
   io $ do
      appData <- readIORef appDataRef
      let actLayerId = AP.activeLayerId appData
          rstate     = ER.RenderState nextFrameFraction $ AP.renderRessources appData

      BG.render $ AP.background appData
      case LE.getL AP.currentLevel appData of
           Just level -> do
              mapM_ (ER.render E.LevelScope rstate) $ LV.entities level
              forM_ (LV.layers level) (\layer ->
                 mapM_ (ER.render (if LY.layerId layer == actLayerId
                                      then E.ActiveLayerScope
                                      else E.InactiveLayerScope) rstate) $ LY.entities layer)
           _          -> return ()


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
         modifyIORef appDataRef (\appData -> appData {AP.windowSize = (width, height)})
         updateFrustum
         updateCamera

      updateFrustum = do
         modifyIORef appDataRef (\appData ->
            let (width, height) = AP.windowSize appData
                top             = orthoScale * (fromIntegral height / fromIntegral width)
                right           = orthoScale
                in appData {AP.frustumSize = (right, top)})

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
