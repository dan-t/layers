
module Event where
import Data.IORef (modifyIORef)
import qualified Data.Lens.Strict as LE
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E
import qualified AppData as AP

type Event = Either AppEvent EntityEvent

data AppEvent = AppEvent

data EntityEvent = UpdateEntity (E.Entity -> E.Entity)


handleEventST :: Event -> AP.AppST ()
handleEventST event = AP.modifyAppST (handleEvent event)


handleEventIO :: Event -> AP.AppDataRef -> IO ()
handleEventIO event appRef = modifyIORef appRef (handleEvent event)


handleEvent :: Event -> AP.AppData -> AP.AppData
handleEvent (Left appEvent) app = app

handleEvent (Right (UpdateEntity f)) app =
   LE.modL AP.currentLevelLens (E.eMap f) app
