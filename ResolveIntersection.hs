
module ResolveIntersection where
import qualified Data.List as L
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import Gamgine.Utils
import qualified GameData.Entity as E
import qualified Entity.Intersect as EI
import qualified Entity.Bound as EB
import qualified Entity.Position as EP
import qualified Entity.Velocity as EV
import qualified Event as EV


resolveIntersection :: EI.Intersection -> Maybe EV.Event
resolveIntersection isect@(e1, e2, isects) =
   case isect of
        (_, _, []) -> Nothing

        (E.Player   {}, E.Platform {}, isects) -> resolvePlayerWithPlatformIsect (e1, isectPositions BT.leaf1) e2
        (E.Platform {}, E.Player   {}, isects) -> resolvePlayerWithPlatformIsect (e2, isectPositions BT.leaf2) e1

        (E.Player {}, E.Star   {}, _) -> resolvePlayerWithStarIsect e1 e2
        (E.Star   {}, E.Player {}, _) -> resolvePlayerWithStarIsect e2 e1

        _ -> Nothing

   where
      isectPositions leaf = L.map (\i -> snd . leaf $ i) isects

      resolvePlayerWithStarIsect _ E.Star {E.starId = isectId} = mkUpdateEntityEvent $ \e ->
         case e of
              E.Star {E.starId = id} | id == isectId -> e {E.starCollected = True}
                                     | otherwise     -> e
              _ -> e

      resolvePlayerWithPlatformIsect (player, isectPos) platform
         | isAnimated platform = moveAwayFromEachOther
         | otherwise           = movePlayerAwayFromPlatform

         where
            movePlayerAwayFromPlatform = mkUpdateEntityEvent $ \e ->
               case e of
                    E.Player {} -> e {E.playerPosition = playerPos',
                                      E.playerOnBottom = onBottom',
                                      E.playerVelocity = playerVelo'}
                    _           -> e
               where
                  playerPos'   = playerPos + V.v3 ox oy 0


            moveAwayFromEachOther = mkUpdateEntityEvent $ \e ->
               case e of
                    E.Player {} -> e {E.playerPosition = playerPos',
                                      E.playerOnBottom = onBottom',
                                      E.playerVelocity = playerVelo'}

                    E.Platform {E.platformId = id} | id == isectId -> EP.updatePosition e platformPos'
                                                   | otherwise     -> e

                    _          -> e
               where
                  playerPos'    = playerPos + V.v3 ox1 oy1 0 + (onBottom ? platformVelo $ V.v3 0 0 0)
                  platformPos'  = platformPos - V.v3 ox2 oy2 0

                  (ox1, oy1) | onBottom  = (ox, oy)
                             | otherwise = (vx1 > 0 ? ox * (vx1/vx) $ 0, vy1 > 0 ? oy * (vy1/vy) $ 0)

                  (ox2, oy2) | onBottom  = (0, 0)
                             | otherwise = (vx2 > 0 ? ox * (vx2/vx) $ 0, vy2 > 0 ? oy * (vy2/vy) $ 0)

                  (vx, vy)      = (vx1 + vx2, vy1 + vy2)
                  (vx1:.vy1:._) = V.map abs playerVelo
                  (vx2:.vy2:._) = V.map abs platformVelo
                  isectId       = E.platformId platform


            playerVelo' = velo (E.playerVelocity player) isectPos
               where
                  velo v           []       = v
                  velo (vx:.vy:._) (ip:ips)
                     | ip == E.Top    = velo (V.v3 vx (-gravity * 5) 0) ips
                     | ip == E.Bottom = velo (V.v3 vx 0 0) ips
                     | otherwise      = velo (V.v3 vx vy 0) ips
                     where
                        gravity = 4.17e-3


            onBottom' = onBottom || E.playerOnBottom player
            onBottom  = L.any (== E.Bottom) isectPos

            playerVelo    = EV.velocity player
            platformVelo  = EV.velocity platform

            playerPos     = EP.currentPosition player
            platformPos   = EP.currentPosition platform

            (ox, oy)      = minOverlapXY $ B.minOverlap playerBound platformBound
               where
                  minOverlapXY (x:.y:._)
                     | ax < ay   = (x, 0)
                     | ay < ax   = (0, y)
                     | otherwise = (x, y)
                     where
                        (ax, ay) = (abs x, abs y)

                  playerBound   = BT.asBox . EB.bound $ player
                  platformBound = BT.asBox . EB.bound $ platform

            isAnimated E.Platform {E.platformPosition = Right _} = True
            isAnimated E.Platform {E.platformPosition = Left  _} = False


      mkUpdateEntityEvent = Just . EV.MkEntityEvent . EV.UpdateEntity