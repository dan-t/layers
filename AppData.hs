
module AppData where
#include "Gamgine/Utils.cpp"
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.IORef as R
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as ST
import qualified Gamgine.Utils as GU
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Entity as E
import qualified Rendering.Ressources as RR
import qualified Updater as UP
import qualified Defaults as DF
import qualified States.State as S
import qualified States.StateTree as SS
import States.StateTree (enterWhen, leaveWhen, adjacents, StateTree(..), StateTransition(..))
import States.StateTreeZipper as SZ
import qualified States.GameRunning as GR
import qualified States.MovingEntity as ME
import qualified States.CreatingPlatform as CP
import qualified States.ResizingPlatform as RP
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI
import qualified States.InputInfo as II
import States.InputInfo (Modifier(..), InputState(..))
IMPORT_LENS

data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   orthoScale       :: Double,
   renderRessources :: RR.Ressources,
   updaters         :: [UP.Updater AppData],
   gameData         :: GD.Data,
   stateTree        :: SZ.Zipper GD.Data
   }

LENS(windowSize)
LENS(frustumSize)
LENS(orthoScale)
LENS(renderRessources)
LENS(updaters)
LENS(gameData)
LENS(stateTree)

newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   orthoScale       = DF.orthoScale,
   renderRessources = RR.Ressources (-1) (-1) (-1),
   updaters         = [],
   gameData         = gameData,
   stateTree        = SZ.zipper $ SS.root GR.mkGameRunningState
      [Branch {state     = ME.mkMovingEntityState,
               enterWhen = ByMouseWithMod GLFW.MouseButton0 Pressed Ctrl,
               leaveWhen = ByMouse GLFW.MouseButton0 Released,
               adjacents = []},
       Branch {state     = RP.mkResizingPlatformState,
               enterWhen = ByMouseWithMod GLFW.MouseButton0 Pressed Shift,
               leaveWhen = ByMouse GLFW.MouseButton0 Released,
               adjacents = []},
       Branch {state     = CP.mkCreatingPlatformState,
               enterWhen = ByMouse GLFW.MouseButton0 Pressed,
               leaveWhen = ByMouse GLFW.MouseButton0 Released,
               adjacents = []}]
   }

data AppMode = GameMode | EditMode deriving Eq

type AppDataRef = R.IORef AppData
type AppST      = ST.StateT AppDataRef IO


runAppST :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runAppST = ST.runStateT


update :: AppData -> AppData
update app = applyToState f app
   where
      f = S.update $ LE.getL currentStateL app


render :: Double -> AppData -> IO AppData
render nextFrameFraction app = applyToStateIO f app
   where
      f      = (S.render $ LE.getL currentStateL app) rstate
      rstate = RR.RenderState nextFrameFraction (renderRessources app)


handleKeyEvent :: KI.KeyInfo -> AppData -> AppData
handleKeyEvent ki app =
   let (gdata', stree') = SZ.handleKeyEvent ki (gameData app) (stateTree app)
       in app {gameData = gdata', stateTree = stree'}


handleMouseEvent :: MI.MouseInfo -> AppData -> AppData
handleMouseEvent mi app =
   let (gdata', stree') = SZ.handleMouseEvent mi (gameData app) (stateTree app)
       in app {gameData = gdata', stateTree = stree'}


handleMouseMoved :: II.MousePos -> AppData -> AppData
handleMouseMoved mp app = applyToState f app
   where
      f = (S.mouseMoved $ LE.getL currentStateL app) mp


applyToState :: (GD.Data -> (GD.Data, S.State GD.Data)) -> AppData -> AppData
applyToState f app = LE.setL currentStateL state' $ app {gameData = gdata'}
   where
      (gdata', state') = f $ gameData app


applyToStateIO :: (GD.Data -> IO (GD.Data, S.State GD.Data)) -> AppData -> IO AppData
applyToStateIO f app = do
   (gdata', state') <- f $ gameData app
   return (LE.setL currentStateL state' $ app {gameData = gdata'})


currentStateL = LE.lens getCurrentState setCurrentState
   where
      getCurrentState = state . SZ.current . stateTree
         where
            state (SS.Branch s _ _ _) = s

      setCurrentState state = LE.modL stateTreeL $ SZ.replace state


currentLevelL  = GD.currentLevelL . gameDataL
activeLayerL   = LV.activeLayerL . currentLevelL
inactiveLayers = LV.inactiveLayers . LE.getL currentLevelL
