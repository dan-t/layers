
module Entity.Entity where
import Control.Applicative ((<$>))
import qualified Event as EV
import qualified Entity.Player as PL
import qualified Entity.Platform as PF
import qualified Entity.Star as ST
import qualified Entity.Scope as SC
import qualified Entity.Bound as B


data Entity = PlayerEntity   PL.Player
            | PlatformEntity PF.Platform
            | StarEntity     ST.Star


update :: Entity -> SC.Scope -> Entity
update (PlayerEntity   pl) s = PlayerEntity   $ PL.update pl s
update (PlatformEntity pf) s = PlatformEntity $ PF.update pf s
update (StarEntity     st) s = StarEntity     $ ST.update st s


render :: Entity -> SC.Scope -> IO Entity
render (PlayerEntity   pl) s = PlayerEntity   <$> PL.render pl s
render (PlatformEntity pf) s = PlatformEntity <$> PF.render pf s
render (StarEntity     st) s = StarEntity     <$> ST.render st s


handleEvent :: Entity -> EV.Event -> SC.Scope -> Entity
handleEvent (PlayerEntity   pl) ev s = PlayerEntity   $ PL.handleEvent pl ev s
handleEvent (PlatformEntity pf) ev s = PlatformEntity $ PF.handleEvent pf ev s
handleEvent (StarEntity     st) ev s = StarEntity     $ ST.handleEvent st ev s


getBound :: Entity -> B.Bound
getBound (PlayerEntity   pl) = PL.getBound pl
getBound (PlatformEntity pf) = PF.getBound pf
getBound (StarEntity     st) = ST.getBound st
