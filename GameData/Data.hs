
module GameData.Data where
import qualified FileData.Data2 as FD
import qualified GameData.Level as LD

data Data entity = Data {
   allLevels   :: FD.Levels,
   activeLevel :: LD.Level entity
   }
