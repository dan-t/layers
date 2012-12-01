
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


atLastLevel :: Data -> Bool
atLastLevel = GZ.atLast . levels


atFirstLevel :: Data -> Bool
atFirstLevel = GZ.atFirst . levels


levelFinished :: Data -> Bool
levelFinished = LV.allStarsCollected . LE.getL currentLevelL


toNextLevel :: Data -> Data
toNextLevel d
   | (LZ.emptyp . levels $ d) || atLastLevel d = d
   | otherwise =
      let (LZ.Zip ls brs, c, LZ.Zip [] (n:ars)) = GZ.split . levels $ d
          (c', n') = LV.changeLevels c n
          in d {levels = LZ.Zip (c':(brs ++ ls)) (n':ars)}


toPreviousLevel :: Data -> Data
toPreviousLevel d
   | (LZ.emptyp . levels $ d) || atFirstLevel d = d
   | otherwise =
      let (LZ.Zip ls [p], c, LZ.Zip [] rs) = GZ.split . levels $ d
          (c', p') = LV.changeLevels c p
          in d {levels = LZ.Zip ls (p':c':rs)}


data AddLevel = BeforeCurrent | AfterCurrent | AfterLast

addEmptyLevel :: AddLevel -> Data -> Data
addEmptyLevel BeforeCurrent = LE.modL levelsL $ LZ.insert LV.newEmptyLevel
addEmptyLevel AfterCurrent  = LE.modL levelsL $ (LZ.insert LV.newEmptyLevel) . LZ.right
addEmptyLevel AfterLast     = LE.modL levelsL $ (LZ.insert LV.newEmptyLevel) . LZ.end


data MoveLevel = Forward | Backward

moveCurrentLevel :: MoveLevel -> Data -> Data
moveCurrentLevel Forward  = LE.modL levelsL $ (applyIf (not . LZ.beginp) LZ.left) . GZ.swapLeft
moveCurrentLevel Backward = LE.modL levelsL $ (applyIf (not . GZ.atLast) LZ.right) . GZ.swapRight


removeCurrentLevel :: Data -> Data
removeCurrentLevel d
   | atFirstLevel d && atLastLevel d = LE.modL levelsL (LZ.replace LV.newEmptyLevel) d
   | otherwise                       = LE.modL levelsL ((applyIf LZ.endp LZ.left) . LZ.delete) d
