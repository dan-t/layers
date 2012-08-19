
module GameData.Data where
import Data.IORef
import Control.Monad.State
import qualified GameData.Level as LV
import qualified Background as BG


data Data = Data {
   windowSize  :: (Int, Int),
   frustumSize :: (Double, Double),
   background  :: BG.Background,
   allLevels   :: [LV.Level],
   activeLevel :: LV.Level
   } deriving Show


newData :: Data
newData = Data {
  windowSize  = (0,0),
  frustumSize = (0,0),
  background  = BG.empty,
  allLevels   = [],
  activeLevel = LV.empty
  }


type DataRef = IORef Data
type DataST  = StateT DataRef IO


runGame :: DataST a -> DataRef -> IO (a, DataRef)
runGame = runStateT
