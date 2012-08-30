
module AppData where
import Data.IORef
import Control.Monad.State
import qualified Background as BG
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified Entity.Render as ER


data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   background       :: BG.Background,
   renderRessources :: ER.Ressources,
   gameData         :: GD.Data,
   currentLevelId   :: Int,
   activeLayerId    :: Int
   }


newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   background       = BG.empty,
   renderRessources = ER.Ressources (-1) (-1),
   gameData         = gameData,
   currentLevelId   = LV.levelId currLevel,
   activeLayerId    = LY.layerId actLayer
   }
   where
      currLevel = GD.levels gameData !! 0
      actLayer  = LV.layers currLevel !! 0


type AppDataRef = IORef AppData
type AppST      = StateT AppDataRef IO


runApp :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runApp = runStateT
