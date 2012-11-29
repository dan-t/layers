
module GameData.Data where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import Gamgine.Control (applyIf)
import qualified Gamgine.Zipper as GZ
import qualified GameData.Level as LV
import qualified GameData.Entity as E
IMPORT_LENS


data Data = Data {
   levels :: LZ.Zipper LV.Level
   }

LENS(levels)

instance E.ApplyToEntity Data where
   eMap f = LE.modL currentLevelL (E.eMap f)
   eFilter p = LE.modL currentLevelL (E.eFilter p)


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


data AddLevel = BeforeCurrent | AfterCurrent | AfterLast

addEmptyLevel :: AddLevel -> Data -> Data
addEmptyLevel BeforeCurrent = LE.modL levelsL $ LZ.insert LV.newEmptyLevel
addEmptyLevel AfterCurrent  = LE.modL levelsL $ (LZ.insert LV.newEmptyLevel) . LZ.right
addEmptyLevel AfterLast     = LE.modL levelsL $ (LZ.insert LV.newEmptyLevel) . LZ.end


data MoveLevel = Forward | Backward

moveCurrentLevel :: MoveLevel -> Data -> Data
moveCurrentLevel Forward  = LE.modL levelsL $ (applyIf (not . LZ.beginp) LZ.left) . GZ.swapWithLeft
moveCurrentLevel Backward = LE.modL levelsL $ (applyIf (not . GZ.atLast) LZ.right) . GZ.swapWithRight
