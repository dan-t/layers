
module GameData.Layer where
import qualified Entity.Entity as E


data Layer = Layer {
   layerId  :: Int,
   entities :: [E.Entity],
   gravity  :: Double
   }


empty :: Layer
empty = Layer 0 [] 0
