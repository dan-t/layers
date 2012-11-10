
module States.StateTree where
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import qualified States.State as S

-- | the tree of application states and how they entered and leaved
data StateTree s = Branch s EnterWhen LeaveWhen [StateTree s]

state :: StateTree s -> s
state (Branch s _ _ _) = s

-- | with the functions root/enter/leave/adjacents/noAdjacents a tree can be specified in the way:
--   root MyRootState `adjacents` [MyState `enter` (ByPress Key GLFW.KeyEsc) `leave` (ByPress Key GLFW.KeyTab) noAdjacents]

root :: s -> ([StateTree s] -> StateTree s)
root s = Branch s NoTransition NoTransition

enter :: s -> EnterWhen -> (LeaveWhen -> [StateTree s] -> StateTree s) 
state `enter` when = Branch state when

leave :: (LeaveWhen -> [StateTree s] -> StateTree s) -> LeaveWhen -> ([StateTree s] -> StateTree s)
f `leave` when = f when

adjacents :: ([StateTree s] -> StateTree s) -> [StateTree s] -> StateTree s
f `adjacents` as = f as

noAdjacents = []


-- | at which event the next state should be entered
type EnterWhen = StateTransition

-- | at which event the current state should be leaved
type LeaveWhen = StateTransition

data Modifier = Ctrl | Alt | Shift deriving (Eq, Ord)

data Input = Key GLFW.Key
           | KeyWithMod GLFW.Key Modifier
           | Mouse GLFW.MouseButton
           | MouseWithMod GLFW.MouseButton Modifier
           deriving (Eq, Ord)

-- | when a state should be entered or leaved
data StateTransition = ByPress Input | ByRelease Input | NoTransition

-- | a zipper for the state tree
data Zipper s = Zipper {
   path    :: [Step s],
   current :: StateTree s
   }

-- | represents a step walking the state tree
data Step s = Step {
   parent   :: (s, EnterWhen, LeaveWhen),
   siblings :: [StateTree s]
   }

zipper :: StateTree s -> Zipper s
zipper s = Zipper [] s


stateTree :: Zipper s -> StateTree s
stateTree z = go z
   where
      go z | atTop z   = current z
           | otherwise = go (goUp z)


replace :: s -> Zipper s -> Zipper s
replace s (Zipper p (Branch _ e l ss)) = Zipper p (Branch s e l ss)


goDownIf :: (EnterWhen -> Bool) -> Zipper s -> Zipper s
goDownIf f z@(Zipper p (Branch c e l ss)) =
   case splitBy f ss of
        (Just st, oss) -> Zipper (Step (c,e,l) oss:p) st
        otherwise      -> z

goUp :: Zipper s -> Zipper s
goUp z@(Zipper [] _) = z
goUp (Zipper (Step (p,e,l) sibs:ps) c) = Zipper ps $ Branch p e l (c:sibs)


atTop :: Zipper s -> Bool
atTop = L.null . path


atBottom :: Zipper s -> Bool
atBottom (Zipper _ (Branch _ _ _ as)) = L.null as


splitBy :: (EnterWhen -> Bool) -> [StateTree s] -> (Maybe (StateTree s), [StateTree s])
splitBy f ss = go f ss []
   where
      go f [] oss = (Nothing, oss)
      go f (s@(Branch _ enter _ _):ss) oss
         | f enter   = (Just s, ss ++ oss)
         | otherwise = go f ss (s:oss)
