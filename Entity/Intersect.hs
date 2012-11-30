
module Entity.Intersect where
import qualified Gamgine.Math.BoxTree as BT
import qualified GameData.Entity as E
import qualified Entity.Bound as EB
import qualified Entity.Id as EI

type EntityId     = Int
type Intersection = (EntityId, EntityId, [BT.Intersection E.Position])


intersect :: E.Entity -> E.Entity -> Maybe Intersection
intersect e1 e2 =
   case (e1, e2) of
        (E.Player   {}, E.Platform {}) -> intersectBounds
        (E.Platform {}, E.Player   {}) -> intersectBounds

        (E.Player                        {}, E.Star   {E.starCollected = False}) -> intersectBounds
        (E.Star   {E.starCollected = False}, E.Player                        {}) -> intersectBounds

        _ -> Nothing
   where
      intersectBounds = case EB.bound e1 `BT.intersection` EB.bound e2 of
                             []     -> Nothing
                             isects -> Just (EI.entityId e1, EI.entityId e2, isects)
