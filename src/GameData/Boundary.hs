
module GameData.Boundary where
#include "Utils.cpp"
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified Entity.Bound as EB
import qualified GameData.Player as PL
import qualified GameData.Animation as A
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import Gamgine.Math.Vect
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB
IMPORT_LENS_AS_LE


-- | the boundary of the whole level
data Boundary = Boundary {box :: B.Box} deriving Show

LENS(box)


newBoundary :: [E.Entity] -> Boundary
newBoundary entities = Boundary boundary
   where
      boundary        = B.Box (V.v3 0 0 0) $ maxArea + V.v3 minBorderDist minBorderDist 0
      B.Box _ maxArea = entitiesArea entities
      minBorderDist   = (max (fst PL.playerSize) (snd PL.playerSize)) * 6


boundaryArea :: Boundary -> V.Vect
boundaryArea (Boundary (B.Box minPt maxPt)) = maxPt - minPt


keepInside :: E.Entity -> Boundary -> E.Entity
player@E.Player {} `keepInside` Boundary bBound@(B.Box minBd@(_:.minY:._) maxBd)
   | pBound `B.inside` bBound = player
   | otherwise                = player {E.playerPosition = pos',
                                        E.playerVelocity = velo',
                                        E.playerOnBottom = onBottom}
   where
      pBound            = (BT.asBox $ E.playerBound player) `B.moveBy` playPos
      pos'              = V.minVec (V.maxVec minBd playPos) maxPos
      velo'             = V.v3 vx vy' vz
      vy' | posY > maxY = 0
          | posY < minY = 0
          | otherwise   = vy

      onBottom | posY < minY = True
               | otherwise   = E.playerOnBottom player

      maxPos@(_:.maxY:._)  = maxBd - V.v3 (fst PL.playerSize) (snd PL.playerSize) 0
      playPos@(_:.posY:._) = E.playerPosition player
      (vx:.vy:.vz:.())     = E.playerVelocity player

keepInside entity _ = entity


entitiesArea :: [E.Entity] -> B.Box
entitiesArea entities = B.bound $ L.map bound entities
   where
      bound E.Platform {E.platformPosition = Right ani, E.platformBound = bound} = pathBound
         where
            pathBound = B.bound $ L.map (bound `B.moveBy`) $ A.path ani

      bound e = BT.asBox $ EB.bound e
