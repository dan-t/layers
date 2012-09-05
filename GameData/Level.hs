
module GameData.Level where
import qualified Data.List as L
import qualified GameData.Layer as LY
import qualified GameData.Entity as E


data Level = Level {
   levelId  :: Int,
   entities :: [E.Entity],
   layers   :: [LY.Layer]
   } deriving Show


sortById :: [Level] -> [Level]
sortById levels = L.sortBy cmpIds levels
   where
      cmpIds Level {levelId = id1} Level {levelId = id2}
         | id1 < id2 = LT
         | id1 > id2 = GT
         | otherwise = EQ


-- | the entities of the level and all its layers
allEntities :: Level -> [E.Entity]
allEntities Level {entities = entities, layers = layers} =
   entities ++ (L.concat $ L.map LY.entities layers)


instance E.ApplyToEntity Level where
   eMap f level = level {entities = E.eMap f $ entities level,
                         layers   = L.map (E.eMap f) $ layers level}
