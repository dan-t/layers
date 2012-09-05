
module Boundary where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified GameData.Player as PL
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import Gamgine.Math.Vect
import qualified Gamgine.Math.BoxTree as BT
import qualified Entity.Bound as EB


-- | the boundary of the whole level
data Boundary = Boundary B.Box


newBoundary :: LV.Level -> Boundary
newBoundary level = Boundary boundary
   where
      boundary         = B.Box (V.v3 0 0 0) $ maxLevel + V.v3 minBorderDist minBorderDist 0
      B.Box _ maxLevel = levelArea level
      minBorderDist    = (max (fst PL.playerSize) (snd PL.playerSize)) * 6


keepInside :: E.Entity -> Boundary -> E.Entity
player@E.Player {} `keepInside` Boundary bBound@(B.Box minBd maxBd)
   | pBound `B.inside` bBound = player
   | otherwise                = player {E.playerPosition = pos', E.playerVelocity = velo'}
   where
      pBound          = (BT.asBox $ E.playerBound player) `B.moveBy` E.playerPosition player
      pos'            = V.minVec minPos maxPos
      velo'           = V.v3 vx 0 vz
      (vx:._:.vz:.()) = E.playerVelocity player
      maxPos          = maxBd - V.v3 (fst PL.playerSize) (snd PL.playerSize) 0
      minPos          = V.maxVec (E.playerPosition player) minBd

keepInside entity _ = entity


levelArea :: LV.Level -> B.Box
levelArea level = B.bound $ L.map (BT.asBox . EB.bound) $ LV.allEntities level
