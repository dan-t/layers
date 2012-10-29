
module Gamgine.Zipper where
import qualified Data.List as L
import qualified Data.List.Zipper as LZ

-- | splits the zipper into the elements before the current element,
--   the current element and the elements after the current element
split :: LZ.Zipper a -> ([a], Maybe a, [a])
split (LZ.Zip ls     []) = (L.reverse ls, Nothing, [])
split (LZ.Zip ls (a:rs)) = (L.reverse ls, Just  a, rs)


-- | map f on the elements of the zipper
map :: (a -> b) -> LZ.Zipper a -> LZ.Zipper b
map f (LZ.Zip ls rs) = LZ.Zip (L.map f ls) (L.map f rs)
