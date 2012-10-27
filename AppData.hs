
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
import qualified Editor as ED
IMPORT_LENS

data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   editor           :: ED.Editor,
   background       :: BG.Background,
   boundary         :: BD.Boundary,
   renderRessources :: RR.Ressources,
   renderers        :: [RD.Renderer],
   gameData         :: GD.Data,
   currentLevelId   :: Int
   }

LENS(windowSize)
LENS(frustumSize)
LENS(editor)
LENS(background)
LENS(boundary)
LENS(renderRessources)
LENS(renderers)
LENS(gameData)
LENS(currentLevelId)

data AppMode = GameMode | EditMode


-- | a lens for the current level
currentLevelL    = currentLevelLens
currentLevelLens = LE.lens getCurrentLevel setCurrentLevel
   where
      getCurrentLevel =
         (\AppData {gameData = gd, currentLevelId = curId} ->
            case L.find ((== curId) . LV.levelId) $ GD.levels gd of
                 Just level -> level
                 _          -> error $ "Couldn't find current level with id=" ++ show curId ++ "!")

      setCurrentLevel =
         (\level@LV.Level {LV.levelId = levId} appData@AppData {gameData = gameData} ->
            appData {currentLevelId = levId,
                     gameData = gameData {GD.levels = GU.replaceBy ((== levId) . LV.levelId) level $ GD.levels gameData}})


newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   editor           = ED.empty,
   background       = BG.empty,
   boundary         = BD.newBoundary currLevel,
   renderRessources = RR.Ressources (-1) (-1),
   renderers        = [],
   gameData         = gameData,
   currentLevelId   = LV.levelId currLevel
   }
   where
      currLevel = GD.levels gameData !! 0


type AppDataRef = R.IORef AppData
type AppST      = ST.StateT AppDataRef IO


runAppST :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runAppST = ST.runStateT
