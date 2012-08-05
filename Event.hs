
module Event where
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B

data Event = MoveEvent   { toPosition :: V.Vect }
           | ResizeEvent { toSize     :: B.Box  }
