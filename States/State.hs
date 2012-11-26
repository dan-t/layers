
module States.State where
import qualified Gamgine.Math.Vect as V
import qualified Rendering.Ressources as RR
import qualified States.InputInfo as II
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI

-- | an application state
data State a = State {
   -- | called when the state is entered,
   --   when Nothing is returned, than the state
   --   couldn't be entered
   enter :: II.MousePos -> a -> Maybe (a, State a),

   -- | called when the state is leaved
   leave :: a -> (a, State a),

   -- | called for each application update cycle
   update :: a -> (a, State a),

   -- | called for each frame rendering
   render :: RR.RenderState -> a -> IO (a, State a),

   -- | called when a key was pressed/released
   keyEvent :: KI.KeyInfo -> a -> (a, State a),

   -- | called when a mouse button was pressed/released
   mouseEvent :: MI.MouseInfo -> a -> (a, State a),

   -- | called when a mouse was moved
   mouseMoved :: II.MousePos -> a -> (a, State a)
   }
