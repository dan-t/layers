
module Gamgine.Bool where

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) m1 m2 = do
   r1 <- m1
   if r1 then m2 else return False


(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) m1 m2 = do
   r1 <- m1
   if r1 then return True else m2

infixr 3 <&&>
infixr 2 <||>


