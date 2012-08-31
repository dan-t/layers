
module AppData where
import qualified Data.IORef as R
import qualified Data.List as L
import qualified Data.Lens.Strict as LE
import qualified Control.Monad.State as S
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
currentLevel =
   LE.lens (\AppData {gameData = gd, currentLevelId = curId} ->
              L.find ((== curId) . LV.levelId) $ GD.levels gd)

           (\maybeLevel ad@AppData {gameData = gd} ->
              maybe ad
                    (\curLevel -> ad {gameData = gd {GD.levels = map (\level ->
                       if LV.levelId level == LV.levelId curLevel
                          then curLevel
                          else level) $ GD.levels gd}})
                    maybeLevel)


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
