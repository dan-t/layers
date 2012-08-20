
module GameData.Level where
import qualified Data.List as L
import qualified GameData.Layer as LY
import qualified GameData.Entity as E


data Level = Level {
   levelId        :: Int,
   entities       :: [E.Entity],
   inactiveLayers :: [LY.Layer],
   activeLayer    :: LY.Layer
   } deriving Show


sortById :: [Level] -> [Level]
sortById levels = L.sortBy cmpIds levels
   where
      cmpIds Level {levelId = id1} Level {levelId = id2}
         | id1 < id2 = LT
         | id1 > id2 = GT
         | otherwise = EQ


empty :: Level
empty = Level 0 [] [] LY.empty
