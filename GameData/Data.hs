
module GameData.Data where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import Gamgine.Utils (applyIf)
import qualified GameData.Level as LV
IMPORT_LENS


data Data = Data {
   levels :: LZ.Zipper LV.Level
   } deriving Show

LENS(levels)

currentLevelL    = currentLevelLens
currentLevelLens = LE.lens getCurrentLevel setCurrentLevel
   where
      getCurrentLevel       = LZ.cursor . levels
      setCurrentLevel level = LE.modL levelsL $ LZ.replace level


newData :: [LV.Level] -> Data
newData = Data . LZ.fromList


allLevels :: Data -> [LV.Level]
allLevels = LZ.toList . levels


toNextLevel :: Data -> Data
toNextLevel = LE.modL levelsL $ \lvs -> applyIf LZ.endp LZ.left $ LZ.right lvs


toPreviousLevel :: Data -> Data
toPreviousLevel = LE.modL levelsL $ applyIf (not . LZ.beginp) LZ.left


addEmptyLevel :: Data -> Data
addEmptyLevel = LE.modL levelsL $ (LZ.insert LV.newEmptyLevel) . LZ.end
