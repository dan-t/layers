
module GameData.Layer where
#include "Gamgine/Utils.cpp"
import Data.Function
import qualified Data.List as L
import qualified GameData.Entity as E
IMPORT_LENS


data Layer = Layer {
   layerId  :: Int,
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show

LENS(layerId)
LENS(entities)
LENS(gravity)

sortById :: [Layer] -> [Layer]
sortById layers = L.sortBy (compare `on` layerId) layers


empty :: Layer
empty = Layer 0 [] 0


instance E.ApplyToEntity Layer where
   eMap f layer = layer {entities = E.eMap f $ entities layer}
