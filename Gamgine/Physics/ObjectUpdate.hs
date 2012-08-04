
module Gamgine.Physics.ObjectUpdate where
import qualified Gamgine.Physics.Object as O
import Gamgine.Math.Vect
import Gamgine.Utils
import Gamgine.Coroutine
import qualified Gamgine.Math.Utils as U
import Debug.Trace

type Gravity = Double
type ObjectUpdate = Coroutine (O.Object, Gravity) O.Object

objectUpdate obj func = (obj, Coroutine func)

identityObjectUpdate :: (O.Object, Gravity) -> (O.Object, ObjectUpdate)
identityObjectUpdate (obj, _) =
   objectUpdate obj identityObjectUpdate

makeIdentityObjectUpdate = Coroutine $ identityObjectUpdate


defaultObjectUpdate :: (O.Object, Gravity) -> (O.Object, ObjectUpdate)
defaultObjectUpdate (obj, gravity) = do
   let v@(vx:.vy:.vz:.()) = O.velocity obj
       vy'                = vy - gravity
       v'                 = v3 vx vy' vz
       p                  = O.position obj
       obj'               = obj {O.velocity = v', O.position = p + v'}
       in objectUpdate obj' defaultObjectUpdate

makeDefaultObjectUpdate = Coroutine $ defaultObjectUpdate
