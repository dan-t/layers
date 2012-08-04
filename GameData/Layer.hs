
module GameData.Layer where
import qualified FileData.Data2 as FD

data Layer entity = Layer {
   id       :: FD.Id,
   entities :: [entity]
   }
