
module GameData.Layer where
import qualified Data.List as L
import qualified GameData.Entity as E


data Layer = Layer {
   layerId  :: Int,
   entities :: [E.Entity],
   gravity  :: Double
   } deriving Show


sortById :: [Layer] -> [Layer]
sortById layers = L.sortBy cmpIds layers
   where
      cmpIds Layer {layerId = id1} Layer {layerId = id2}
         | id1 < id2 = LT
         | id1 > id2 = GT
         | otherwise = EQ


empty :: Layer
empty = Layer 0 [] 0


initRessources :: Layer -> IO Layer
initRessources layer = do
   es' <- mapM E.initRessources $ entities layer
   return layer {entities = es'}


render :: E.Scope -> Layer -> IO ()
render scope layer = mapM_ (E.render scope) $ entities layer
