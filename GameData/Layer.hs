
module GameData.Layer where
import qualified GameData.Entity as E


data Layer = Layer {
   layerId  :: Int,
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show


instance Eq Layer where
   l1 == l2 =
      layerId l1 == layerId l2


instance Ord Layer where
   l1 <= l2 =
      layerId l1 <= layerId l2


empty :: Layer
empty = Layer 0 [] 0
