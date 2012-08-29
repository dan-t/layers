
module GameData.Data where
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.List as L
import Control.Monad.State
import qualified GameData.Level as LV
import qualified Entity.Render as ER
import qualified Background as BG


data Data = Data {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   background       :: BG.Background,
   otherLevels      :: [LV.Level],
   currentLevel     :: LV.Level,
   renderRessources :: ER.Ressources
   } deriving Show


newData :: Data
newData = Data {
  windowSize   = (0,0),
  frustumSize  = (0,0),
  background   = BG.empty,
  otherLevels  = [],
  currentLevel = LV.empty,
  renderRessources = ER.Ressources (-1) (-1)
  }


type DataRef = IORef Data
type DataST  = StateT DataRef IO


runGame :: DataST a -> DataRef -> IO (a, DataRef)
runGame = runStateT


nextLevel :: Data -> Data
nextLevel dat@Data {currentLevel = clv@LV.Level {LV.levelId = cid}, otherLevels = lvs} =
   maybe dat

         (\nlv@LV.Level {LV.levelId = nid} ->
            dat {currentLevel = nlv,
                 otherLevels  = LV.sortById $ clv : L.filter ((/= nid) . LV.levelId) lvs})

         (L.find ((> cid) . LV.levelId) lvs)
