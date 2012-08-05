
module GameData.Animation where
import Gamgine.Math.Vect

data Animation = Animation {
   velocity        :: Double,
   path            :: [Vect],
   bidirectional   :: Bool,
   movingToPathIdx :: Int,
   finished        :: Bool
   }

animation :: Double -> [Vect] -> Bool -> Animation
animation velo path bidir = Animation velo path bidir 0 False
