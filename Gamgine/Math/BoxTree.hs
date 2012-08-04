
module Gamgine.Math.BoxTree where
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import Data.List (concat)
import Debug.Trace

data BoxTree a = Node B.Box [BoxTree a]
		  | Leaf B.Box a
		  deriving (Show)

-- intersection details
data Intersection a = Intersection {
   leaf1 :: (B.Box, a),
   leaf2 :: (B.Box, a)
   }
   deriving (Show)

intersection :: BoxTree a -> BoxTree a -> [Intersection a]
qt1 `intersection` qt2 = qt1 `isect` qt2
   where
      isect :: BoxTree a -> BoxTree a -> [Intersection a]

      (Node b1 qts1) `isect` (Node b2 qts2)
	 | b1 `B.intersects` b2 = concat $ [qt1 `isect` qt2 | qt1 <- qts1, qt2 <- qts2]
	 | otherwise            = []

      l@(Leaf b1 _) `isect` (Node b2 qts2)
	 | b1 `B.intersects` b2 = concat $ [l `isect` qt2 | qt2 <- qts2]
	 | otherwise            = []

      (Node b1 qts1) `isect` l@(Leaf b2 _)
	 | b1 `B.intersects` b2 = concat $ [qt1 `isect` l | qt1 <- qts1]
	 | otherwise            = []

      (Leaf b1 a1) `isect` (Leaf b2 a2)
--	 | b1 `B.intersects` b2 = trace ("b1: " ++ show b1 ++ "\nb2: " ++ show b2) [Intersection (b1, a1) (b2, a2)]
	 | b1 `B.intersects` b2 = [Intersection (b1, a1) (b2, a2)]
	 | otherwise            = []


moveBy :: BoxTree a -> V.Vect -> BoxTree a
(Node b qts) `moveBy` v = Node (b `B.moveBy` v) [qt `moveBy` v | qt <- qts]
(Leaf b a)   `moveBy` v = Leaf (b `B.moveBy` v) a


asBox :: BoxTree a -> B.Box
asBox (Node box _) = box
asBox (Leaf box _) = box

asBoxTree :: B.Box -> a -> BoxTree a
asBoxTree b a = Leaf b a
