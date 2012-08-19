
module Entity.Platform where
import qualified Data.List as L
import qualified FileData.Data2 as FD
import qualified GameData.Animation as A
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import Gamgine.Math.Vect
import qualified Event as EV
import qualified Entity.Entity as E

type PositionOrAnimation = Either Vect A.Animation

data Platform = Platform {
   platformId :: Int,
   position   :: PositionOrAnimation,
   bound      :: B.Box
   } deriving Show


instance E.ToFileEntity Platform where
   toFileEntity Platform {platformId = id, position = posOrAnim, bound = bound} =
      FD.Platform id (toPosOrAnimation posOrAnim) (B.toTuples bound)

toPosOrAnimation :: PositionOrAnimation -> FD.PositionOrAnimation
toPosOrAnimation (Left  pos)  = Left $ V.toTuple pos
toPosOrAnimation (Right anim) = Right $ toAnimation anim

toAnimation :: A.Animation -> FD.Animation
toAnimation anim = FD.Animation velo path bidir
   where
      velo  = A.velocity anim
      path  = L.map V.toTuple (A.path anim)
      bidir = A.bidirectional anim


instance E.EntityT Platform where
   update e s = e
   render e s = return e
   handleEvent e ev s = e
   getBound e = Nothing

newPlatform :: Int -> PositionOrAnimation -> B.Box -> Platform
newPlatform id posOrAnim bound = Platform id posOrAnim bound
