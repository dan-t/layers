
module Gamgine.Control where
import Data.Bool.HT (if')

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = do
   r <- p
   if r then m else return ()


-- | if a is just than apply f, otherwise 'return ()'
ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust (Just a) f = f a
ifJust _        _ = return ()

-- | apply f on a if p is true, otherwise just return a
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f a | p a       = f a
              | otherwise = a

-- | apply f on a if p is true, otherwise 'return ()'
applyIfM :: Monad m => (a -> Bool) -> (a -> m ()) -> a -> m ()
applyIfM p f a | p a       = f a
               | otherwise = return ()


(?) :: Bool -> a -> a -> a
(?) = if'
infixl 1 ?
