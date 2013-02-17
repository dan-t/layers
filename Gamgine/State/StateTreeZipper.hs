
module Gamgine.State.StateTreeZipper where
import qualified Data.List as L
import Data.List ((!!))
import qualified Gamgine.State.State as S
import qualified Gamgine.State.StateTree as ST
import Gamgine.State.StateTree (root, enterWhen, leaveWhen, adjacents, StateTree(..), StateTransition(..))
import qualified Gamgine.State.KeyInfo as KI
import qualified Gamgine.State.MouseInfo as MI
import qualified Gamgine.State.InputInfo as II

-- | a zipper for the state tree
data Zipper a = Zipper {
   path    :: [Step a],
   current :: ST.StateTree a
   }


-- | represents a step walking the state tree
data Step a = Step {
   parent   :: (S.State a, ST.EnterWhen, ST.LeaveWhen),
   -- (beforeSiblings, afterSiblings)
   siblings :: ([ST.StateTree a], [ST.StateTree a])
   }


-- | create a zipper from a StateTree
zipper :: ST.StateTree a -> Zipper a
zipper s = Zipper [] s


-- | checks if a state transition should occur, otherwise
--   calls the keyEvent function of the current state
handleKeyEvent :: KI.KeyInfo -> a -> Zipper a -> (a, Zipper a)
handleKeyEvent ki a z@(Zipper ps c)
   | transitionAppliesKI ki (ST.leaveWhen . current $ z) = goUp mp a z
   | otherwise = 
      case L.findIndex (== True) $ L.map (transitionAppliesKI ki . ST.enterWhen) (ST.adjacents . current $ z) of
           Just idx -> goDown idx mp a z
           _        -> let (a', c') = ST.handleKeyEvent ki a c in (a', Zipper ps c') 
   where
      mp = KI.mousePos ki


-- | checks if a state transition should occur, otherwise
--   calls the mouseEvent function of the current state
handleMouseEvent :: MI.MouseInfo -> a -> Zipper a -> (a, Zipper a)
handleMouseEvent mi a z@(Zipper ps c)
   | transitionAppliesMI mi (ST.leaveWhen . current $ z) = goUp mp a z
   | otherwise = 
      case L.findIndex (== True) $ L.map (transitionAppliesMI mi . ST.enterWhen) (ST.adjacents . current $ z) of
           Just idx -> goDown idx mp a z
           _        -> let (a', c') = ST.handleMouseEvent mi a c in (a', Zipper ps c') 
   where
      mp = MI.mousePos mi
      
   
-- | leave the current state and enter the parent state
goUp :: II.MousePos -> a -> Zipper a -> (a, Zipper a)
goUp mp a z@(Zipper [] _) = (a, z)
goUp mp a z@(Zipper (Step (p,e,l) (beforeSibs, afterSibs):ps) c) =
   case (S.enter p) mp a' of
        Just (a'', p') -> (a'', Zipper ps $ ST.Branch p' e l (beforeSibs ++ (c' : afterSibs)))
        _              -> (a, z)
   where
      (a' , c') = ST.leaveState a c


-- | leave the current state and enter the adjacent state
--   with index 'adjIdx'
goDown :: Int -> II.MousePos -> a -> Zipper a -> (a, Zipper a)
goDown adjIdx mp a z@(Zipper ps (Branch c e l as))
   | adjIdx >= L.length as = (a, z)
   | otherwise =
      case ST.enterState mp a' s of
           Just (a'', s') -> (a'', Zipper ((Step (c',e,l) (beforeSibs, afterSibs)):ps) s')
           _              -> (a, z)
   where
      (a', c')   = (S.leave c) a
      s          = as !! adjIdx
      beforeSibs = L.take adjIdx as
      afterSibs  = L.drop (adjIdx + 1) as


-- | replace the current state
replace :: S.State a -> Zipper a -> Zipper a
replace s (Zipper p (ST.Branch _ e l ss)) = Zipper p (ST.Branch s e l ss)


-- | checks if the KeyInfo matches the StateTransition
transitionAppliesKI :: KI.KeyInfo -> StateTransition -> Bool
transitionAppliesKI ki (ByKey key status) =
   KI.key ki == key && KI.status ki == status

transitionAppliesKI ki (ByKeyWithMod key status mod) =
   KI.key ki == key && KI.status ki == status && L.any (== mod) (KI.modifiers ki)

transitionAppliesKI _ _ = False


-- | checks of the MouseInfo matches the StateTransition
transitionAppliesMI :: MI.MouseInfo -> StateTransition -> Bool
transitionAppliesMI mi (ByMouse button status) =
   MI.button mi == button && MI.status mi == status

transitionAppliesMI mi (ByMouseWithMod button status mod) =
   MI.button mi == button && MI.status mi == status && L.any (== mod) (MI.modifiers mi)

transitionAppliesMI _ _ = False
