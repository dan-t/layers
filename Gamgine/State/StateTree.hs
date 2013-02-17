
module Gamgine.State.StateTree where
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import qualified Gamgine.State.State as S
import qualified Gamgine.State.InputInfo as II
import qualified Gamgine.State.MouseInfo as MI
import qualified Gamgine.State.KeyInfo as KI

-- | the tree of application states and how they entered and leaved
data StateTree a = Branch {
   state     :: S.State a,
   enterWhen :: StateTransition,
   leaveWhen :: StateTransition,
   adjacents :: [StateTree a]
   }

-- | at which event the next state should be entered
type EnterWhen = StateTransition

-- | at which event the current state should be leaved
type LeaveWhen = StateTransition

-- | when a state should be entered or leaved
data StateTransition = ByKey GLFW.Key II.InputState
                     | ByKeyWithMod GLFW.Key II.InputState II.Modifier
                     | ByMouse GLFW.MouseButton II.InputState
                     | ByMouseWithMod GLFW.MouseButton II.InputState II.Modifier
                     | NoTransition
                     deriving (Eq, Ord)

root :: S.State a -> [StateTree a] -> StateTree a
root s as = Branch s NoTransition NoTransition as


enterState :: II.MousePos -> a -> StateTree a -> Maybe (a, StateTree a)
enterState mp a st@(Branch s e l as) =
   case (S.enter s) mp a of
        Just (a', s') -> Just (a', Branch s' e l as)
        _             -> Nothing


leaveState :: a -> StateTree a -> (a, StateTree a)
leaveState a (Branch s e l as) =
   let (a', s') = S.leave s $ a in (a', Branch s' e l as)


handleKeyEvent :: KI.KeyInfo -> a -> StateTree a -> (a, StateTree a)
handleKeyEvent ki a (Branch s e l as) =
   let (a', s') = (S.keyEvent s) ki a in (a', Branch s' e l as)


handleMouseEvent :: MI.MouseInfo -> a -> StateTree a -> (a, StateTree a)
handleMouseEvent mi a (Branch s e l as) =
   let (a', s') = (S.mouseEvent s) mi a in (a', Branch s' e l as)
