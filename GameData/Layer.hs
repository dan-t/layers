
module GameData.Layer where
import qualified GameData.Entity as E

data Layer = Layer {
   layerId  :: Int,
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show

empty :: Layer
empty = Layer 0 [] 0
