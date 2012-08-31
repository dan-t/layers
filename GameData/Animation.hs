
module GameData.Animation where
import qualified Data.List as L
import Gamgine.Math.Vect as V


data Animation = Animation {
   currentPosition :: V.Vect,
   velocity        :: Double,
   path            :: [V.Vect],
   bidirectional   :: Bool,
   movingToPathIdx :: Int,
   finished        :: Bool
   } deriving Show


newAnimation :: Double -> [V.Vect] -> Bool -> Animation
newAnimation velo p@(currPos : _) bidir = Animation currPos velo p bidir 1 False


update :: Animation -> Animation
update ani@Animation {finished = True} = ani

update ani@Animation {currentPosition = currPos, velocity = velo, path = path, movingToPathIdx = idx} =
   let nextPos | idx >= L.length path = currPos
               | otherwise            = path !! idx

       dir            = V.normalize $ nextPos - currPos
       currPos'       = currPos + (dir * V.v3 velo velo velo)
       nextPosReached = V.len (currPos' - nextPos) < 0.1
       in if nextPosReached
             then incrementIdx ani {currentPosition = nextPos}
             else ani {currentPosition = currPos'}
   where
      incrementIdx ani@Animation {movingToPathIdx = idx, path = path, bidirectional = bidir} =
         let nextIdx = idx + 1
             atEnd   = nextIdx >= L.length path
             (path', nextIdx', fin) | atEnd && bidir = (L.reverse path, 1, False)
                                    | not atEnd      = (path, nextIdx, False)
                                    | otherwise      = (path, 1, True)
             in ani {path = path', movingToPathIdx = nextIdx', finished = fin}
