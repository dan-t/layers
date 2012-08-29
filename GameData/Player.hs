
module GameData.Player where
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E


newPlayer :: Int -> V.Vect -> E.Entity
newPlayer id initPos = E.Player {
   E.playerId         = id,
   E.playerInitialPos = initPos,
   E.playerPosition   = initPos,
   E.playerVelocity   = V.v3 0 0 0,
   E.playerOnBottom   = False,
   E.playerMovement   = E.AtRest,
   E.playerBound      = playerBound playerSize
   }


playerSize :: (Double, Double)
playerSize = (2, 2.5)


playerBound :: (Double, Double) -> E.Bound
playerBound (x, y) =
   let frac1      = 1/20
       frac2      = 1 - frac1
       topBox     = B.Box (V.v3 (frac1*x) (frac2*y) 0) (V.v3 (frac2*x) (1*y) 0)
       bottomBox  = B.Box (V.v3 (frac1*x) 0 0)         (V.v3 (frac2*x) (frac1*y) 0)
       leftBox    = B.Box (V.v3 0 (frac1*y) 0)         (V.v3 (frac1*x) (frac2*y) 0)
       rightBox   = B.Box (V.v3 (frac2*x) (frac1*y) 0) (V.v3 (1*x) (frac2*y) 0)
       middleBox  = B.Box (V.v3 (frac1*x) (frac1*y) 0) (V.v3 (frac2*x) (frac2*y) 0)
       topLeaf    = BT.Leaf topBox E.Top
       bottomLeaf = BT.Leaf bottomBox E.Bottom
       leftLeaf   = BT.Leaf leftBox E.Left
       rightLeaf  = BT.Leaf rightBox E.Right
       middleLeaf = BT.Leaf middleBox E.Whatever
       in BT.Node (B.Box (V.v3 0 0 0) (V.v3 x y 0))
                  [leftLeaf, topLeaf, rightLeaf, bottomLeaf, middleLeaf]
