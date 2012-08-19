
module GameData.Level where
import qualified GameData.Layer as LY
import qualified GameData.Entity as E


data Level = Level {
   levelId        :: Int,
   entities       :: [E.Entity],
   inactiveLayers :: [LY.Layer],
   activeLayer    :: LY.Layer
   } deriving Show


instance Eq Level where
   l1 == l2 = 
      levelId l1 == levelId l2


instance Ord Level where
   l1 <= l2 = 
      levelId l1 <= levelId l2


empty :: Level
empty = Level 0 [] [] LY.empty
