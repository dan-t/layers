
module GameData.Data where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import qualified GameData.Level as LV
IMPORT_LENS


data Data = Data {
   levels :: LZ.Zipper LV.Level
   } deriving Show

LENS(levels)

currentLevelL    = currentLevelLens
currentLevelLens = LE.lens getCurrentLevel setCurrentLevel
   where
      getCurrentLevel dat       = LZ.cursor . levels $ dat
      setCurrentLevel level dat = LE.modL levelsL (LZ.replace level) dat


newData :: [LV.Level] -> Data
newData levels = Data $ LZ.fromList levels


allLevels :: Data -> [LV.Level]
allLevels dat = LV.sortById . LZ.toList . levels $ dat


switchToNextLevel :: Data -> Data
switchToNextLevel d@Data {levels = lvs}
   | not . LZ.endp $ lvs = d {levels = LZ.right lvs}
   | otherwise           = d


switchToPreviousLevel :: Data -> Data
switchToPreviousLevel d@Data {levels = lvs}
   | not . LZ.beginp $ lvs = d {levels = LZ.left lvs}
   | otherwise             = d


atLastLevel :: Data -> Bool
atLastLevel dat = LZ.endp . levels $ dat
