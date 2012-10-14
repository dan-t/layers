
module Gamgine.Coroutine where

newtype Coroutine i o = Coroutine {
   runCoroutine :: i -> (o, Coroutine i o)
   }

newtype CoroutineM m i o = CoroutineM {
   runCoroutineM :: i -> m (o, CoroutineM m i o)
   }
