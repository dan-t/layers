{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}

module States.State where
import qualified Gamgine.Math.Vect as V
import qualified Rendering.Ressources as RR
import qualified States.InputInfo as II
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI

-- | an application state
class State a b where
   -- | called once when the state is entered
   enter :: a -> b -> (a, b)
   enter a b = (a, b)

   -- | alternative enter state function with the
   --   current mouse position
   enterWithMousePos :: a -> b -> II.MousePos -> (a, b)
   enterWithMousePos a b _ = enter a b

   -- | called once when the state is leaved
   leave :: a -> b -> (a, b)
   leave a b = (a, b)

   -- | called for each application update cycle
   update :: a -> b -> (a, b)
   update a b = (a, b)

   -- | called for each frame rendering
   render :: a -> b -> RR.RenderState -> IO (a, b)
   render a b _ = return (a, b)

   -- | called when a key was pressed/released
   keyEvent :: a -> b -> KI.KeyInfo -> (a, b)
   keyEvent a b _ = (a, b)

   -- | called when a mouse button was pressed/released
   mouseEvent :: a -> b -> MI.MouseInfo -> (a, b)
   mouseEvent a b _ = (a, b)

   -- | called when a mouse was moved
   mouseMoved :: a -> b -> II.MousePos -> (a, b)
   mouseMoved a b _ = (a, b)


-- | wrapper for holding any state
data AnyState a = forall s. State s a => AnyState s
