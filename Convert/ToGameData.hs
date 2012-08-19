
module Convert.ToGameData where
import qualified Data.List as L
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified GameData.Entity as E
import qualified FileData.Data2 as FD
import qualified GameData.Player as P
import qualified GameData.Star as S
import qualified GameData.Platform as PF
import qualified GameData.Animation as A
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified GameData.Data as GD


toGameData :: FD.Data -> GD.Data
toGameData (FD.Data _ [])     = GD.newData
toGameData (FD.Data _ levels) = GD.newData {
   GD.otherLevels  = othLevels,
   GD.currentLevel = curLevel
   }
   where
      (curLevel : othLevels) = L.sort $ L.map toLevel levels


toLevel :: FD.Level -> LV.Level
toLevel (FD.Level id entities layers) = LV.Level {
   LV.levelId        = id,
   LV.entities       = gameEntities,
   LV.inactiveLayers = inactLayers,
   LV.activeLayer    = actLayer
   }
   where
      gameEntities             = L.map toEntity entities
      (actLayer : inactLayers) = L.sort $ L.map toLayer layers


toLayer :: FD.Layer -> LY.Layer
toLayer (FD.Layer id entities gravity) = LY.Layer id gameEntities gravity
   where
      gameEntities = L.map toEntity entities


toEntity :: FD.Entity -> E.Entity
toEntity (FD.Player id initPos) =
   E.Entity $ P.newPlayer id (V.fromTuple initPos)

toEntity (FD.Platform id posOrAnim bound) =
   E.Entity $ PF.newPlatform id (toPosOrAnimation posOrAnim) (B.fromTuples bound)

toEntity (FD.Star id pos) = 
   E.Entity $ S.newStar id (V.fromTuple pos)


toPosOrAnimation :: FD.PositionOrAnimation -> PF.PositionOrAnimation
toPosOrAnimation (Left  pos)  = Left $ V.fromTuple pos
toPosOrAnimation (Right anim) = Right $ toAnimation anim


toAnimation :: FD.Animation -> A.Animation
toAnimation (FD.Animation velo fpath bidir) = A.animation velo path bidir
   where
      path = L.map V.fromTuple fpath
