
module Entity.Bound where
import qualified Data.List as L
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Entity.Position as EP

bound :: E.Entity -> E.Bound
bound E.Player {E.playerPosition = pos, E.playerBound = bound} =
   bound `BT.moveBy` pos

bound e@E.Enemy {E.enemyBound = bound} =
   BT.Leaf (bound `B.moveBy` EP.currentPosition e) E.Whatever

bound p@E.Platform {E.platformBound = bound} =
   BT.Leaf (bound `B.moveBy` EP.currentPosition p) E.Whatever

bound E.Star {E.starPosition = pos, E.starBound = bound} =
   BT.Leaf (bound `B.moveBy` pos) E.Whatever
