
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


-- | if the current element is the last valid one
atLast :: LZ.Zipper a -> Bool
atLast (LZ.Zip _ (a:[])) = True
atLast _                 = False


-- | if the current element is the first valid one
atFirst :: LZ.Zipper a -> Bool
atFirst (LZ.Zip [] (a:ls)) = True
atFirst _                  = False


-- | swap the current element with the left one
swapWithLeft :: LZ.Zipper a -> LZ.Zipper a
swapWithLeft z@(LZ.Zip     []      _) = z
swapWithLeft z@(LZ.Zip      _     []) = z
swapWithLeft   (LZ.Zip (l:ls) (c:rs)) = LZ.Zip (c:ls) (l:rs)


-- | swap the current element with the right one
swapWithRight :: LZ.Zipper a -> LZ.Zipper a
swapWithRight z@(LZ.Zip  _       []) = z
swapWithRight z@(LZ.Zip  _   (c:[])) = z
swapWithRight   (LZ.Zip ls (c:r:rs)) = LZ.Zip ls (r:c:rs)
