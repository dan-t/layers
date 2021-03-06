
module Convert.ToFileData where
import qualified Data.List as L
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified FileData.Data2 as FD
import qualified GameData.Entity as E
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified GameData.Data as D
import qualified GameData.Animation as A


toFileData :: D.Data -> FD.Data
toFileData dat =
   FD.Data FD.dataVersion $ L.map toLevel (D.allLevels dat)


toLevel :: LV.Level -> FD.Level
toLevel l@LV.Level {LV.entities = entities} =
   FD.Level fileEntities fileLayers
   where
      fileEntities = L.map toEntity entities
      fileLayers   = L.map toLayer $ LV.allLayers l


toLayer :: LY.Layer -> FD.Layer
toLayer LY.Layer {LY.entities = entities, LY.gravity = g} = FD.Layer fileEntities g
   where
      fileEntities = L.map toEntity entities


toEntity :: E.Entity -> FD.Entity
toEntity E.Player {E.playerId = id, E.playerInitialPos = initPos} =
   FD.Player id (V.toTuple initPos)

toEntity E.Enemy {E.enemyId = id, E.enemyPosition = pos} =
   FD.Enemy id (toPosOrAnimation pos)

toEntity E.Platform {E.platformId = id, E.platformPosition = pos, E.platformBound = bound} =
   FD.Platform id (toPosOrAnimation pos) (B.toTuples bound)

toEntity E.Star {E.starId = id, E.starPosition = pos} =
   FD.Star id (V.toTuple pos)


toPosOrAnimation :: E.PositionOrAnimation -> FD.PositionOrAnimation
toPosOrAnimation (Left  pos)  = Left $ V.toTuple pos
toPosOrAnimation (Right anim) = Right $ toAnimation anim

toAnimation :: A.Animation -> FD.Animation
toAnimation anim = FD.Animation velo path bidir
   where
      velo  = A.velocity anim
      path  = L.map V.toTuple (A.path anim)
      bidir = A.bidirectional anim
