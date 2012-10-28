
module AppData where
#include "Gamgine/Utils.cpp"
import qualified Data.IORef as R
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as ST
import qualified Gamgine.Utils as GU
import qualified Background as BG
import qualified Boundary as BD
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified Rendering.Ressources as RR
import qualified Rendering.Renderer as RD
import qualified Updater as UP
import qualified Defaults as DF
IMPORT_LENS

data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   orthoScale       :: Double,
   background       :: BG.Background,
   boundary         :: BD.Boundary,
   renderRessources :: RR.Ressources,
   renderers        :: [RD.Renderer],
   updaters         :: [UP.Updater AppData],
   gameData         :: GD.Data
   }

LENS(windowSize)
LENS(frustumSize)
LENS(orthoScale)
LENS(background)
LENS(boundary)
LENS(renderRessources)
LENS(renderers)
LENS(updaters)
LENS(gameData)

data AppMode = GameMode | EditMode deriving Eq


currentLevelL   = GD.currentLevelL . gameDataL
activeLayerL    = LV.activeLayerL . currentLevelL
inactiveLayersL = LV.inactiveLayersL . currentLevelL


newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   orthoScale       = DF.orthoScale,
   background       = BG.empty,
   boundary         = BD.newBoundary $ GD.currentLevel gameData,
   renderRessources = RR.Ressources (-1) (-1),
   renderers        = [],
   updaters         = [],
   gameData         = gameData
   }


type AppDataRef = R.IORef AppData
type AppST      = ST.StateT AppDataRef IO


runAppST :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runAppST = ST.runStateT
