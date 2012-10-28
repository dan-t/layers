
module GameData.Data where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import Gamgine.Utils (mapz, applyIf)
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


toNextLevel :: Data -> Data
toNextLevel d@Data {levels = lvs}
   | not . atLastLevel $ d = d {levels = LZ.right lvs}
   | otherwise             = d


toPreviousLevel :: Data -> Data
toPreviousLevel d@Data {levels = lvs}
   | not . atFirstLevel $ d = d {levels = LZ.left lvs}
   | otherwise              = d


addEmptyLevel :: Data -> Data
addEmptyLevel dat = LE.modL levelsL (\levels ->
   if LZ.emptyp levels
      then LZ.insert (LV.newEmptyLevel 1) levels
      else LZ.insert (LV.newEmptyLevel $ lastLevelId + 1) $ LZ.end levels)
   dat
   where
      lastLevelId = LV.levelId . LZ.cursor . LZ.left . LZ.end . levels $ dat


atFirstLevel :: Data -> Bool
atFirstLevel Data {levels = levels} = (not . LZ.emptyp $ levels) && LZ.beginp levels


atLastLevel :: Data -> Bool
atLastLevel Data {levels = levels} = (not . LZ.emptyp $ levels) && (LZ.endp $ LZ.right levels)
