
module GameData.Enemy where
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified GameData.Entity as E
import qualified Defaults as DF


newEnemy :: Int -> E.PositionOrAnimation -> E.Entity
newEnemy id posOrAnim = E.Enemy {
   E.enemyId        = id,
   E.enemyPosition  = posOrAnim,
   E.enemyBound     = enemyBound,
   E.enemyWalkCycle = (0,6),
   E.enemyLiving    = True
   }


enemySize :: (Double, Double)
enemySize = (2, 1.8)


enemyBound :: B.Box
enemyBound = B.Box V.nullVec (V.v3 x y 0)
   where
      (x, y) = enemySize


enemyVelocity :: Double
enemyVelocity = 5 / (fromIntegral DF.ticksPerSecond)
