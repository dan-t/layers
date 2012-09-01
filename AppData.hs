
module AppData where
import qualified Data.IORef as R
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Lens.Strict as LE
import qualified Control.Monad.State as S
import qualified Gamgine.Utils as GU
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


-- | a lens for the current level
currentLevel = LE.lens getCurrentLevel setCurrentLevel
   where
      getCurrentLevel =
         (\AppData {gameData = gd, currentLevelId = curId} ->
            L.find ((== curId) . LV.levelId) $ GD.levels gd)

      setCurrentLevel =
         (\maybeLevel ad@AppData {gameData = gd} ->
            case maybeLevel of
                 Just level@LV.Level {LV.levelId = levId} ->
                    ad {currentLevelId = levId,
                        gameData = gd {GD.levels = GU.replaceBy ((== levId) . LV.levelId) level $ GD.levels gd}}
                 _          -> ad)


-- | a lens for the active layer
activeLayer = LE.lens getActiveLayer setActiveLayer
   where
      getActiveLayer =
         (\ad@AppData {activeLayerId = actId} -> do
            level <- LE.getL currentLevel ad
            L.find ((== actId) . LY.layerId) $ LV.layers level)

      setActiveLayer =
         (\maybeLayer appData ->
            fromMaybe
               appData
               (do layer <- maybeLayer
                   level <- LE.getL currentLevel appData
                   let layId  = LY.layerId layer
                       level' = Just $ level {LV.layers = GU.replaceBy ((== layId) . LY.layerId) layer $ LV.layers level}
                   Just $ LE.setL currentLevel level' appData {activeLayerId = layId}))


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


type AppDataRef = R.IORef AppData
type AppST      = S.StateT AppDataRef IO


runApp :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runApp = S.runStateT
