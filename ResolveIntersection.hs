
module ResolveIntersection where
import qualified GameData.Entity as E
import qualified Entity.Intersect as EI
import qualified Event as EV


resolveIntersection :: EI.Intersection -> Maybe EV.Event
resolveIntersection isect@(e1, e2, isects) =
   case isect of
        (_, _, []) -> Nothing

        (E.Player   {}, E.Platform {}, isects) -> playerWithPlatform e1 e2
        (E.Platform {}, E.Player   {}, isects) -> playerWithPlatform e2 e1

        (E.Player {}, E.Star   {}, _) -> playerWithStar e1 e2
        (E.Star   {}, E.Player {}, _) -> playerWithStar e2 e1

        _ -> Nothing

   where
      playerWithPlatform player platform = Nothing

      playerWithStar _ E.Star {E.starId = isectId} = Just $ EV.MkEntityEvent $ EV.UpdateEntity $ \e ->
         case e of
              E.Star {E.starId = id} | id == isectId -> e {E.starCollected = True}
                                     | otherwise     -> e
              _ -> e
