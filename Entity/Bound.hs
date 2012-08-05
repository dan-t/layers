
module Entity.Bound where
import qualified Gamgine.Math.BoxTree as BT

data Position      = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)
type Bound         = BT.BoxTree Position
type Intersections = [BT.Intersection Position]
