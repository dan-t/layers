
module Entity.Bound where
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B

bound :: E.Entity -> E.Bound
bound E.Player {E.playerPosition = pos, E.playerBound = bound} =
   bound `BT.moveBy` pos

bound E.Platform {E.platformPosition = Left pos, E.platformBound = bound} =
   BT.Leaf (bound `B.moveBy` pos) E.Whatever

bound E.Platform {E.platformPosition = Right ani, E.platformBound = bound} =
   BT.Leaf (B.bound pathBounds) E.Whatever
   where
      pathBounds = L.map (bound `B.moveBy`) $ A.path ani

bound E.Star {E.starPosition = pos, E.starBound = bound} =
   BT.Leaf (bound `B.moveBy` pos) E.Whatever
