
module GameData.Level where
import qualified FileData.Data2 as FD
import qualified GameData.Layer as L

data Level entity = Level {
   id          :: FD.Id,
   area        :: (Double, Double),
   entities    :: [entity],
   allLayers   :: [FD.Layer],
   activeLayer :: L.Layer entity
   }
