
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


-- | split the zipper in a zipper of the before elements,
--   the current element and a zipper of the after elements
split :: LZ.Zipper a -> (LZ.Zipper a, a, LZ.Zipper a)
split z = (before z, LZ.cursor z, after z)


-- | if the current element is the last valid one
atLast :: LZ.Zipper a -> Bool
atLast (LZ.Zip _ (a:[])) = True
atLast _                 = False


-- | if the current element is the first valid one
atFirst :: LZ.Zipper a -> Bool
atFirst (LZ.Zip [] (a:ls)) = True
atFirst _                  = False


-- | swap the current element with the left one
swapLeft :: LZ.Zipper a -> LZ.Zipper a
swapLeft z@(LZ.Zip     []      _) = z
swapLeft z@(LZ.Zip      _     []) = z
swapLeft   (LZ.Zip (l:ls) (c:rs)) = LZ.Zip (c:ls) (l:rs)


-- | swap the current element with the right one
swapRight :: LZ.Zipper a -> LZ.Zipper a
swapRight z@(LZ.Zip  _       []) = z
swapRight z@(LZ.Zip  _   (c:[])) = z
swapRight   (LZ.Zip ls (c:r:rs)) = LZ.Zip ls (r:c:rs)
