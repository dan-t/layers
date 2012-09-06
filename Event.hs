
module Event where
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E

type Event = Either DataEvent EntityEvent

data DataEvent = DataEvent

data EntityEvent = PlayerStartsMoving E.Movement
                 | PlayerStopsMoving E.Movement
                 | PlayerJumping
                 | MoveEntity {entityId :: Int, toPosition :: V.Vect}
                 | ResizePlatform {platformId :: Int, toSize :: B.Box}
