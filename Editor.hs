
module Editor where
#include "Gamgine/Utils.cpp"
import Gamgine.Math.Vect
IMPORT_LENS

data Editor = Editor {
   editing   :: Edition,
   scrolling :: Vect,
   zoom      :: Int
   } deriving (Show)

LENS(editing)
LENS(scrolling)
LENS(zoom)

empty :: Editor
empty = Editor NoEdition (v3 0 0 0) 0

type Origin   = Vect
type ObjectId = Int
type MaxPoint = Vect
type Position = Vect
type Path     = [Vect]

data Edition = CreatePlatform Origin
                 | ResizePlatform MaxPoint Origin ObjectId
                 | DefineMovingPath Path ObjectId
                 | MoveObject Position Origin ObjectId
                 | Scroll Origin
                 | NoEdition deriving (Show)
