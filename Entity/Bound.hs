
module Entity.Bound where
import qualified GameData.Entity as E
import qualified Gamgine.Math.BoxTree as BT

bound :: E.Entity -> E.Bound
bound E.Player {E.playerBound = bound} = bound

bound E.Platform {E.platformBound = bound} = BT.Leaf bound E.Whatever

bound E.Star {E.starBound = bound} = BT.Leaf bound E.Whatever
