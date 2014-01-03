
module GameData.Star where
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E


newStar :: Int -> V.Vect -> E.Entity
newStar id pos = E.Star {
   E.starId        = id,
   E.starPosition  = pos,
   E.starBound     = starBound starSize,
   E.starCollected = False
   }


starSize :: (Double, Double)
starSize = (1.5, 1.5)


starBound :: (Double, Double) -> B.Box
starBound (x, y) = B.Box (V.v3 0 0 0) (V.v3 x y 0)
