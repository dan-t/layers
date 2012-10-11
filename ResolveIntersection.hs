
module ResolveIntersection where
import qualified Data.List as L
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import qualified GameData.Entity as E
import qualified Entity.Intersect as EI
import qualified Entity.Bound as EB
import qualified Event as EV


resolveIntersection :: EI.Intersection -> Maybe EV.Event
resolveIntersection isect@(e1, e2, isects) =
   case isect of
        (_, _, []) -> Nothing

        (E.Player   {}, E.Platform {}, isects) -> playerWithPlatform (e1, isectPositions BT.leaf1) e2
        (E.Platform {}, E.Player   {}, isects) -> playerWithPlatform (e2, isectPositions BT.leaf2) e1

        (E.Player {}, E.Star   {}, _) -> playerWithStar e1 e2
        (E.Star   {}, E.Player {}, _) -> playerWithStar e2 e1

        _ -> Nothing

   where
      isectPositions leaf = L.map (\i -> snd . leaf $ i) isects

      playerWithStar _ E.Star {E.starId = isectId} = mkUpdateEntityEvent $ \e ->
         case e of
              E.Star {E.starId = id} | id == isectId -> e {E.starCollected = True}
                                     | otherwise     -> e
              _ -> e

      playerWithPlatform (player, isectPos) platform
         | isAnimated platform = moveAwayFromEachOther
         | otherwise           = movePlayerAwayFromPlatform

         where
            movePlayerAwayFromPlatform = mkUpdateEntityEvent $ \e ->
               case e of
                    E.Player {E.playerPosition = pos} -> e {E.playerPosition = pos + displacement,
                                                            E.playerOnBottom = onBottom',
                                                            E.playerVelocity = velocity'}
                    _ -> e
               where
                  displacement = V.v3 minx miny 0
                  (minx, miny) = minOverlapXY $ B.minOverlap playBound platBound
                  playBound    = BT.asBox . EB.bound $ player
                  platBound    = BT.asBox . EB.bound $ platform

            moveAwayFromEachOther = Nothing

            isAnimated E.Platform {E.platformPosition = Left  _} = False
            isAnimated E.Platform {E.platformPosition = Right _} = True

            velocity' = velo (E.playerVelocity player) isectPos
               where
                  velo v           []       = v
                  velo (vx:.vy:._) (ip:ips)
                     | ip == E.Top    = velo (V.v3 vx (-gravity * 5) 0) ips
                     | ip == E.Bottom = velo (V.v3 vx 0 0) ips
                     | otherwise      = velo (V.v3 vx vy 0) ips
                     where
                        gravity = 4.17e-3

            onBottom' = L.any (== E.Bottom) isectPos || E.playerOnBottom player

            minOverlapXY (x:.y:._)
               | ax < ay   = (x, 0)
               | ay < ax   = (0, y)
               | otherwise = (x, y)
               where
                  (ax, ay) = (abs x, abs y)


      mkUpdateEntityEvent = Just . EV.MkEntityEvent . EV.UpdateEntity
