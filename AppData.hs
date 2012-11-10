
module AppData where
#include "Gamgine/Utils.cpp"
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
import qualified States.GameRunning as GR
IMPORT_LENS

data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   orthoScale       :: Double,
   renderRessources :: RR.Ressources,
   updaters         :: [UP.Updater AppData],
   gameData         :: GD.Data,
   appState         :: SS.Zipper (S.AnyState GD.Data)
   }

LENS(windowSize)
LENS(frustumSize)
LENS(orthoScale)
LENS(renderRessources)
LENS(updaters)
LENS(gameData)
LENS(appState)


data AppMode = GameMode | EditMode deriving Eq

currentState = SS.state . SS.current . appState

update :: AppData -> AppData
update app =
   case currentState app of
        (S.AnyState state) ->
           let (state', gdata') = S.update state $ gameData app
               in app {appState = SS.replace (S.AnyState state') $ appState app,
                       gameData = gdata'}


render :: Double -> AppData -> IO AppData
render nextFrameFraction app =
   case currentState app of
        (S.AnyState state) -> do
           let rstate = RR.RenderState nextFrameFraction (renderRessources app)
           (state', gdata') <- S.render state (gameData app) rstate
           return app {appState = SS.replace (S.AnyState state') $ appState app,
                       gameData = gdata'}


currentLevelL  = GD.currentLevelL . gameDataL
activeLayerL   = LV.activeLayerL . currentLevelL
inactiveLayers = LV.inactiveLayers . LE.getL currentLevelL


newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   orthoScale       = DF.orthoScale,
   renderRessources = RR.Ressources (-1) (-1) (-1),
   updaters         = [],
   gameData         = gameData,
   appState         = SS.zipper $ SS.root (S.AnyState GR.GameRunning) []
   }


type AppDataRef = R.IORef AppData
type AppST      = ST.StateT AppDataRef IO


runAppST :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runAppST = ST.runStateT
