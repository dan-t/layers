
module GameData.Data where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified GameData.Level as LV
IMPORT_LENS


data Data = Data {
   finishedLevels  :: [LV.Level],
   currentLevel     :: LV.Level,
   followingLevels :: [LV.Level]
   } deriving Show

LENS(finishedLevels)
LENS(currentLevel)
LENS(followingLevels)


newData :: [LV.Level] -> Data
newData (curLv : folLvs) = Data [] curLv folLvs


allLevels :: Data -> [LV.Level]
allLevels Data {finishedLevels = finLvs, currentLevel = curLv, followingLevels = folLvs} =
   LV.sortById $ finLvs ++ [curLv] ++ folLvs


switchToNextLevel :: Data -> Data
switchToNextLevel d@Data {followingLevels = []} = d
switchToNextLevel d@Data {finishedLevels = finLvs, currentLevel = curLv, followingLevels = folLvs} =
   d {finishedLevels  = finLvs ++ [curLv],
      currentLevel    = L.head folLvs,
      followingLevels = L.tail folLvs}


atLastLevel :: Data -> Bool
atLastLevel Data {followingLevels = []} = True
atLastLevel _                           = False
