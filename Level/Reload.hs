
module Level.Reload where
import qualified GameData.Level as LV
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG

reload :: LV.Level -> LV.Level
reload level =
   let reloaded       = TG.toLevel . TF.toLevel $ level
       (_, reloaded') = LV.changeLevels level reloaded
       in reloaded' {LV.boundary = LV.boundary level}
