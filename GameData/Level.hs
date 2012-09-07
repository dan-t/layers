
module GameData.Level where
import Data.Function
import qualified Data.List as L
import qualified GameData.Layer as LY
import qualified GameData.Entity as E


data Level = Level {
   levelId  :: Int,
   entities :: [E.Entity],
   layers   :: [LY.Layer]
   } deriving Show


sortById :: [Level] -> [Level]
sortById levels = L.sortBy (compare `on` levelId) levels


-- | the entities of the level and all its layers
allEntities :: Level -> [E.Entity]
allEntities Level {entities = entities, layers = layers} =
   entities ++ (L.concat $ L.map LY.entities layers)


instance E.ApplyToEntity Level where
   eMap f level = level {entities = E.eMap f $ entities level,
                         layers   = L.map (E.eMap f) $ layers level}
