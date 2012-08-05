
module GameData.Data where
import qualified GameData.Level as LD

data Data = Data {
   allLevels   :: [LD.Level],
   activeLevel :: LD.Level
   }
