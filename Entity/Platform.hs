
module Entity.Platform where
import qualified GameData.Animation as A
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect
import qualified Event as EV
import qualified Entity.Entity as E

type PositionOrAnimation = Either Vect A.Animation

data Platform = Platform {
   platformId :: Int,
   position   :: PositionOrAnimation,
   bound      :: B.Box
   } deriving Show

instance E.EntityT Platform where
   update e s = e
   render e s = return e
   handleEvent e ev s = e
   getBound e = Nothing

newPlatform :: Int -> PositionOrAnimation -> B.Box -> Platform
newPlatform id posOrAnim bound = Platform id posOrAnim bound
