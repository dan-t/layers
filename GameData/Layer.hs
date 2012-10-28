
module GameData.Layer where
#include "Gamgine/Utils.cpp"
import qualified GameData.Entity as E
IMPORT_LENS


data Layer = Layer {
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show

LENS(entities)
LENS(gravity)


instance E.ApplyToEntity Layer where
   eMap    f layer = layer {entities = E.eMap f $ entities layer}
   eFilter p layer = layer {entities = E.eFilter p $ entities layer}
