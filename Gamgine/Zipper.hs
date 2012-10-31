
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
