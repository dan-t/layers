
module GameData.Entity where
import qualified GameData.PhysicalAttrs as P

class Entity a where
   update   :: a -> a
   render   :: a -> IO a
   getBound :: a -> Maybe P.Bound
