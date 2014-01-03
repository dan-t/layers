
module Event where
import Data.IORef (modifyIORef)
import qualified Data.List as L
import qualified Data.Lens.Strict as LE
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E
import qualified GameData.Level as LV

data Event = MkLevelEvent LevelEvent
             | MkEntityEvent EntityEvent
             | MkMultiEvent [Event]

data LevelEvent = UpdateLevel (LV.Level -> LV.Level)

data EntityEvent = UpdateEntity (E.Entity -> E.Entity)

handleEvents :: [Event] -> LV.Level -> LV.Level
handleEvents events lv =
   L.foldl' (flip handleEvent) lv events

handleEvent :: Event -> LV.Level -> LV.Level
handleEvent (MkLevelEvent (UpdateLevel f)) lv = f lv

handleEvent (MkEntityEvent (UpdateEntity f)) lv =
   E.eMap f lv

handleEvent (MkMultiEvent es) lv =
   L.foldl' (flip handleEvent) lv es
