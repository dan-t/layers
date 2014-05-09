
module GameData.Data where
#include "Utils.cpp"
import qualified Data.List as L
import qualified Data.List.Zipper as LZ
import Gamgine.Control (applyIf)
import qualified Gamgine.Zipper as GZ
import qualified GameData.Level as LV
import qualified GameData.Entity as E
IMPORT_LENS_AS_LE


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


gameFinished :: Data -> Bool
gameFinished d = levelFinished d && atLastLevel d


toNextLevel :: Data -> Data
toNextLevel d@Data {levels = lvs}
   | LZ.emptyp lvs || GZ.atLast lvs = d
   | otherwise =
      let lvs      = levels d
          (c', n') = LV.changeLevels (GZ.current lvs) (GZ.next lvs)
          in d {levels = LZ.replace n' . LZ.right . LZ.replace c' $ lvs}


toPreviousLevel :: Data -> Data
toPreviousLevel d@Data {levels = lvs}
   | LZ.emptyp lvs || GZ.atFirst lvs = d
   | otherwise =
      let (c', p') = LV.changeLevels (GZ.current lvs) (GZ.previous lvs)
          in d {levels = LZ.replace p' . LZ.left . LZ.replace c' $ lvs}


data AddLevel = BeforeCurrent | AfterCurrent | AfterLast

addEmptyLevel :: AddLevel -> Data -> Data
addEmptyLevel BeforeCurrent d@Data {levels = lvs} =
   let (c', nlv') = LV.changeLevels (GZ.current lvs) LV.newEmptyLevel
       in d {levels = LZ.insert nlv' . LZ.replace c' $ lvs}

addEmptyLevel AfterCurrent d@Data {levels = lvs} =
   let (c', nlv') = LV.changeLevels (GZ.current lvs) LV.newEmptyLevel
       in d {levels = LZ.insert nlv' . LZ.right . LZ.replace c' $ lvs}

addEmptyLevel AfterLast d@Data {levels = lvs} =
   let (c', nlv') = LV.changeLevels (GZ.current lvs) LV.newEmptyLevel
       in d {levels = LZ.insert nlv' . LZ.end . LZ.replace c' $ lvs}


data MoveLevel = Forward | Backward

moveCurrentLevel :: MoveLevel -> Data -> Data
moveCurrentLevel Forward d@Data {levels = lvs}
   | LZ.beginp lvs = d
   | otherwise     =
      let (p, c) = (GZ.previous lvs, GZ.current lvs)
          in d {levels = LZ.replace c . LZ.left . LZ.replace p $ lvs}

moveCurrentLevel Backward d@Data {levels = lvs}
   | GZ.atLast lvs = d
   | otherwise     =
      let (c, n) = (GZ.current lvs, GZ.next lvs)
          in d {levels = LZ.replace c . LZ.right . LZ.replace n $ lvs}


removeCurrentLevel :: Data -> Data
removeCurrentLevel d@Data {levels = lvs}
   | GZ.atFirst lvs && GZ.atLast lvs =
      let (_, nlv') = LV.changeLevels (GZ.current lvs) LV.newEmptyLevel
          in d {levels = LZ.replace nlv' lvs}

   | otherwise = d {levels = applyIf LZ.endp LZ.left . LZ.delete $ lvs}
