
module Convert.ToFileData where
import qualified Data.List as L
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified Entity.Entity as E
import qualified FileData.Data2 as FD
import qualified Entity.Player as P
import qualified Entity.Star as S
import qualified Entity.Platform as PF
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified GameData.Data as D
import qualified GameData.Animation as A


toFileData :: D.Data -> FD.Data
toFileData (D.Data lvs _) = FD.Data FD.dataVersion (L.map toLevel lvs)


toLevel :: LV.Level -> FD.Level
toLevel (LV.Level id es ls _) = FD.Level id fes fls
   where
      fes = L.map toEntity es
      fls = L.map toLayer ls


toLayer :: LY.Layer -> FD.Layer
toLayer (LY.Layer id es g) = FD.Layer id fes g
   where
      fes = L.map toEntity es


toEntity :: E.Entity -> FD.Entity
toEntity (E.PlayerEntity pl) = FD.Player id initPos
   where
      id      = P.playerId pl
      initPos = V.toTuple $ P.initialPosition pl

toEntity (E.PlatformEntity pf) = FD.Platform id posOrAnim bound
   where
      id        = PF.platformId pf
      posOrAnim = toPosOrAnimation $ PF.position pf
      bound     = B.toTuples $ PF.bound pf

toEntity (E.StarEntity st) = FD.Star (S.starId st) (V.toTuple $ S.position st)


toPosOrAnimation :: PF.PositionOrAnimation -> FD.PositionOrAnimation
toPosOrAnimation (Left  pos)  = Left $ V.toTuple pos
toPosOrAnimation (Right anim) = Right $ toAnimation anim


toAnimation :: A.Animation -> FD.Animation
toAnimation anim = FD.Animation velo path bidir
   where
      velo  = A.velocity anim
      path  = L.map V.toTuple (A.path anim)
      bidir = A.bidirectional anim
