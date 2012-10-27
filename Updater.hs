
module Updater where
import qualified Gamgine.Coroutine as CO

type Finished = Bool

-- | a update routine which is called/used until it returns Finished=True
--   used for temporary data manipulations (e.g moving of entities)
type Updater a = CO.CoroutineM IO a (a, Finished)

runUpdater = CO.runCoroutineM

contineUpdater result update = (result, CO.CoroutineM $ update)

mkUpdater update = CO.CoroutineM update
