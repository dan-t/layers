
module GameData.Star where
import qualified Gamgine.Math.Vect as V
import qualified FileData.Data2 as FD
import qualified Event as EV
import qualified GameData.Entity as E

data Star = Star {
   starId    :: Int,
   position  :: V.Vect,
   collected :: Bool
   } deriving Show

instance E.ToFileEntity Star where
   toFileEntity Star {starId = id, position = pos} =
      FD.Star id (V.toTuple pos)

instance E.EntityT Star where
   update e s = e
   render e s = return ()
   handleEvent e ev s = e
   getBound e = Nothing

newStar :: Int -> V.Vect -> Star
newStar id pos = Star id pos False
