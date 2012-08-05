
module Entity.Star where
import qualified Gamgine.Math.Vect as V
import qualified Event as EV
import qualified Entity.Scope as SC
import qualified Entity.Bound as BN

data Star = Star {
   starId    :: Int,
   position  :: V.Vect,
   collected :: Bool
   }

newStar :: Int -> V.Vect -> Star
newStar id pos = Star id pos False

update e s = e
render e s = return e
handleEvent e ev s = e
getBound e = undefined
