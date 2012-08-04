
module GameData.PhysicalAttrs where
import Gamgine.Math.Vect
import qualified Gamgine.Math.BoxTree as BT

data Position      = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)
type Bound         = BT.BoxTree Position
type Intersections = [BT.Intersection Position]

data PhysicalAttrs = PhysicalAttrs {
   velocity :: Vect,
   position :: Vect,
   bound    :: Bound
   }

integrate :: PhysicalAttrs -> Double -> PhysicalAttrs
integrate (PhysicalAttrs (vx:.vy:.vz:.()) pos bound) gravity = PhysicalAttrs velo' pos' bound
   where
      pos'  = pos + velo'
      velo' = v3 vx vy' vz
      vy'   = vy - gravity
