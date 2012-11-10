
module Event where
import Data.IORef (modifyIORef)
import qualified Data.List as L
import qualified Data.Lens.Strict as LE
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified Gamgine.IORef as GR
import qualified GameData.Entity as E
import qualified GameData.Data as GD

data Event = MkGameEvent GameEvent
             | MkEntityEvent EntityEvent
             | MkMultiEvent [Event]

data GameEvent = UpdateGame (GD.Data -> GD.Data)

data EntityEvent = UpdateEntity (E.Entity -> E.Entity)

handleEvents :: [Event] -> GD.Data -> GD.Data
handleEvents events gd =
   L.foldl' (flip handleEvent) gd events

handleEvent :: Event -> GD.Data -> GD.Data
handleEvent (MkGameEvent (UpdateGame f)) gd = f gd

handleEvent (MkEntityEvent (UpdateEntity f)) gd =
   LE.modL GD.currentLevelL (E.eMap f) gd

handleEvent (MkMultiEvent es) gd =
   L.foldl' (flip handleEvent) gd es
