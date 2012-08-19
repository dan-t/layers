
module GameData.Level where
import qualified GameData.Layer as LY
import qualified GameData.Entity as E

data Level = Level {
   levelId     :: Int,
   entities    :: [E.Entity],
   allLayers   :: [LY.Layer],
   activeLayer :: LY.Layer
   } deriving Show

empty :: Level
empty = Level 0 [] [] LY.empty
