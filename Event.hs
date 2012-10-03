
module Event where
import Data.IORef (modifyIORef)
import qualified Data.List as L
import qualified Data.Lens.Strict as LE
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E
import qualified AppData as AP

data Event = MkAppEvent AppEvent
             | MkEntityEvent EntityEvent
             | MkMultiEvent [Event]

data AppEvent = UpdateApp (AP.AppData -> AP.AppData)

data EntityEvent = UpdateEntity (E.Entity -> E.Entity)


handleEventST :: Event -> AP.AppST ()
handleEventST event = AP.modifyAppST (handleEvent event)


handleEventIO :: Event -> AP.AppDataRef -> IO ()
handleEventIO event appRef = modifyIORef appRef (handleEvent event)


handleEvent :: Event -> AP.AppData -> AP.AppData
handleEvent (MkAppEvent (UpdateApp f)) app = f app

handleEvent (MkEntityEvent (UpdateEntity f)) app =
   LE.modL AP.currentLevelLens (E.eMap f) app

handleEvent (MkMultiEvent es) app =
   L.foldl' (flip handleEvent) app es
