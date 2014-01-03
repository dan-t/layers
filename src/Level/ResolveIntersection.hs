
module Level.ResolveIntersection where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import Gamgine.Control ((?))
import qualified GameData.Entity as E
import qualified GameData.Level as LV
import qualified GameData.Enemy as EN
import qualified Entity.Intersect as EI
import qualified Entity.Bound as EB
import qualified Entity.Position as EP
import qualified Entity.Velocity as EV
import qualified Level.Reload as LR
import qualified Event as EV
import qualified Rendering.Renderer as RR
import qualified Defaults as DF
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG
IMPORT_LENS_AS_LE


resolveIntersection :: EI.Intersection -> LV.Level -> Maybe EV.Event
resolveIntersection isect@(e1, e2, isects) level =
   resolve (LV.findEntityById e1 level) (LV.findEntityById e2 level) isects
   where
      resolve Nothing _ _ = ERROR("Couldn't find entity with id " ++ show e1)
      resolve _ Nothing _ = ERROR("Couldn't find entity with id " ++ show e2)
      resolve _ _ []      = Nothing

      resolve (Just e1) (Just e2) isects =
         case (e1, e2, isects) of
              (_, _, []) -> Nothing

              (E.Player   {}, E.Platform {}, isects) -> resolvePlayerWithPlatformIsect (e1, isectPositions BT.leaf1) e2
              (E.Platform {}, E.Player   {}, isects) -> resolvePlayerWithPlatformIsect (e2, isectPositions BT.leaf2) e1

              (E.Player {}, E.Enemy  {}, isects) -> resolvePlayerWithEnemyIsect (e1, isectPositions BT.leaf1) e2
              (E.Enemy  {}, E.Player {}, isects) -> resolvePlayerWithEnemyIsect (e2, isectPositions BT.leaf2) e1

              (E.Player {}, E.Star   {}, _) -> resolvePlayerWithStarIsect e1 e2
              (E.Star   {}, E.Player {}, _) -> resolvePlayerWithStarIsect e2 e1

              _ -> Nothing
         where
            isectPositions leaf = L.map (\i -> snd . leaf $ i) isects


      resolvePlayerWithStarIsect _ E.Star {E.starId = isectId, E.starPosition = pos} = Just . mkMultiEvent $ [
         mkUpdateEntityEvent $ \e ->
            case e of
                 E.Star {E.starId = id} | id == isectId -> e {E.starCollected = True}
                                        | otherwise     -> e
                 _ -> e,

         mkUpdateLevelEvent $ LE.modL LV.renderersL $ \rds ->
            RR.mkRenderer (RR.fadeOutStar pos 0) : rds ]


      resolvePlayerWithEnemyIsect (player, isectPos) enemy
         | playerKilled = Just . mkUpdateLevelEvent $ LR.reload
         | otherwise    = Just . mkMultiEvent $ [
            mkUpdateEntityEvent $ \e ->
               case e of
                    E.Enemy {E.enemyId = id} | id == enemyId -> e {E.enemyLiving = False}
                                             | otherwise     -> e
                    _ -> e,

            mkUpdateLevelEvent $ LE.modL LV.renderersL $ \rds ->
               RR.mkRenderer (RR.fadeOutEnemy enemyPos EN.enemySize) : rds ]
         where
            playerKilled = L.any (/= E.Bottom) isectPos
            enemyId      = E.enemyId enemy
            enemyPos     = EP.currentPosition enemy


      resolvePlayerWithPlatformIsect (player, isectPos) platform
         | isAnimated platform = moveAwayFromEachOther
         | otherwise           = movePlayerAwayFromPlatform

         where
            movePlayerAwayFromPlatform = Just . mkUpdateEntityEvent $ \e ->
               case e of
                    E.Player {} -> e {E.playerPosition = playerPos',
                                      E.playerOnBottom = onBottom',
                                      E.playerVelocity = playerVelo'}
                    _           -> e
               where
                  playerPos'   = playerPos + V.v3 ox oy 0


            moveAwayFromEachOther = Just . mkUpdateEntityEvent $ \e ->
               case e of
                    E.Player {} -> e {E.playerPosition = playerPos',
                                      E.playerOnBottom = onBottom',
                                      E.playerVelocity = playerVelo'}

                    E.Platform {E.platformId = id} | id == isectId -> EP.setCurrentPosition e platformPos'
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
                     | ip == E.Top    = velo (V.v3 vx (-DF.gravity * 5) 0) ips
                     | ip == E.Bottom = velo (V.v3 vx 0 0) ips
                     | otherwise      = velo (V.v3 vx vy 0) ips


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

      mkMultiEvent        = EV.MkMultiEvent
      mkUpdateLevelEvent  = EV.MkLevelEvent  . EV.UpdateLevel
      mkUpdateEntityEvent = EV.MkEntityEvent . EV.UpdateEntity
