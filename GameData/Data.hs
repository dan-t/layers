
module GameData.Data where
import qualified GameData.Level as LV


data Data = Data {
   levels :: [LV.Level]
   } deriving Show
