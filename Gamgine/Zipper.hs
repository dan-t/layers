
module Gamgine.Zipper where
import qualified Data.List as L
import qualified Data.List.Zipper as LZ


-- | get the elements before the current one
before :: LZ.Zipper a -> LZ.Zipper a
before (LZ.Zip     [] _) = LZ.empty
before (LZ.Zip (a:ls) _) = LZ.Zip ls [a]


-- | get the elements after the current one
after :: LZ.Zipper a -> LZ.Zipper a
after (LZ.Zip _     []) = LZ.empty
after (LZ.Zip _ (a:rs)) = LZ.Zip [] rs


-- | get the current element, might fail
current :: LZ.Zipper a -> a
current = LZ.cursor


-- | get the previous element, might fail
previous :: LZ.Zipper a -> a
previous (LZ.Zip (p:ls) _) = p


-- | get the next element, might fail
next :: LZ.Zipper a -> a
next (LZ.Zip _ (c:n:_)) = n


-- | if the current element is the last of the list
atLast :: LZ.Zipper a -> Bool
atLast (LZ.Zip _ (a:[])) = True
atLast _                 = False


-- | if the current element is the first of the list
atFirst :: LZ.Zipper a -> Bool
atFirst (LZ.Zip [] (a:ls)) = True
atFirst _                  = False
