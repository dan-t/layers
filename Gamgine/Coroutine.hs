
module Gamgine.Coroutine where
import Control.Monad

newtype Coroutine i o = Coroutine {
   runCoroutine :: i -> (o, Coroutine i o)
   }

newtype (Monad m) => CoroutineM m i o = CoroutineM {
   runCoroutineM :: i -> m (o, CoroutineM m i o)
   }
