
module Convert.ToFileData where
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified FileData.Data2 as FD
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
import qualified GameData.Data as D


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
toEntity (E.Entity e) = E.toFileEntity e
