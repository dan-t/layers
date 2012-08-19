
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
import qualified GameData.Data as D


toGameData :: FD.Data -> D.Data
toGameData (FD.Data _ [])         = D.Data [] LV.empty
toGameData (FD.Data _ (flv:flvs)) = D.Data (lv : lvs) lv
   where
      lv  = toLevel flv
      lvs = L.map toLevel flvs


toLevel :: FD.Level -> LV.Level
toLevel (FD.Level id fes (fl:fls)) = LV.Level id es (l : ls) l
   where
      es = L.map toEntity fes
      l  = toLayer fl
      ls = L.map toLayer fls


toLayer :: FD.Layer -> LY.Layer
toLayer (FD.Layer id fes g) = LY.Layer id es g
   where
      es = L.map toEntity fes


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
