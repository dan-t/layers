
module GameData.Layer where
#include "Utils.cpp"
import qualified GameData.Entity as E
import qualified Defaults as DF
IMPORT_LENS_AS_LE


data Layer = Layer {
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show

LENS(entities)
LENS(gravity)


newEmptyLayer :: Layer
newEmptyLayer = Layer [] DF.gravity


instance E.ApplyToEntity Layer where
   eMap    f layer = layer {entities = E.eMap f $ entities layer}
   eFilter p layer = layer {entities = E.eFilter p $ entities layer}
