
module Entity.Platform where
import qualified GameData.Animation as A
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect

type PositionOrAnimation = Either Vect A.Animation

data Platform = Platform {
   platformId :: Int,
   position   :: PositionOrAnimation,
   bound      :: B.Box
   }

update e s = e
render e s = return e
handleEvent e ev s = e
getBound e = undefined
