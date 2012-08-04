{-# LANGUAGE GADTs #-}

module Collision (Collision) where
import qualified GameData.PhysicalAttrs as P
import qualified Event as EV
import qualified Gamgine.Math.BoxTree as BT

class (a ~ b) => Collision a b where
   resolveAB :: a -> b -> P.Intersections -> (a, b, EV.Event)
   resolveAB a b isects = resolveBA b a (swap isects)

   resolveBA :: b -> a -> P.Intersections -> (b, a, EV.Event)
   resolveBA b a isects = resolveAB a b (swap isects) 


swap isects = 
   map (\(BT.Intersection l1 l2) -> BT.Intersection l2 l1) isects 
