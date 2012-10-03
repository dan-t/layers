
module Entity.Intersect where
import qualified Data.List as L
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified GameData.Entity as E
import qualified Entity.Bound as EB

type Intersection = (E.Entity, E.Entity, [BT.Intersection E.Position])


intersect :: E.Entity -> E.Entity -> Intersection
intersect e1 e2 =
   case (e1, e2) of
        (E.Player   {}, E.Platform {}) -> intersectBounds
        (E.Platform {}, E.Player   {}) -> intersectBounds

        (E.Player {}                       , E.Star   {E.starCollected = False}) -> intersectBounds
        (E.Star   {E.starCollected = False}, E.Player {})                        -> intersectBounds

        _ -> (e1, e2, [])
   where
      intersectBounds = (e1, e2, boundIsects)
         where
            boundIsects = EB.bound e1 `BT.intersection` EB.bound e2
