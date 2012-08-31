
module GameData.Animation where
import qualified Data.List as L
import Gamgine.Math.Vect as V


data Animation = Animation {
   currentPosition  :: V.Vect,
   currentDirection :: V.Vect,
   velocity         :: Double,
   path             :: [V.Vect],
   bidirectional    :: Bool,
   movingToPathIdx  :: Int,
   finished         :: Bool
   } deriving Show


newAnimation :: Double -> [V.Vect] -> Bool -> Animation
newAnimation velocity path@(currPos : nextPos : _) bidirectional =
   Animation currPos currDir velocity path bidirectional 1 False
   where
      currDir = V.normalize $ nextPos - currPos


update :: Animation -> Animation
update ani@Animation {finished = True} = ani

update ani@Animation {currentPosition = currPos, currentDirection = currDir, velocity = velo, path = path, movingToPathIdx = idx} =
   let nextPos | idx >= L.length path = currPos
               | otherwise            = path !! idx

       currPos'       = currPos + (currDir * V.v3 velo velo velo)
       nextPosReached = V.len (currPos' - nextPos) < 0.1
       in if nextPosReached
             then incrementIdx ani {currentPosition = nextPos}
             else ani {currentPosition = currPos'}
   where
      incrementIdx ani@Animation {currentPosition = currPos, movingToPathIdx = idx, path = path, bidirectional = bidir} =
         let nextIdx = idx + 1
             atEnd   = nextIdx >= L.length path
             in if atEnd
                   then if bidir then newAnimation velo (L.reverse path) bidir else ani {finished = True}
                   else ani {movingToPathIdx = nextIdx, currentDirection = V.normalize $ (path !! nextIdx) - currPos}
