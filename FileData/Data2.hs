
module FileData.Data2 where

dataVersion :: Int
dataVersion = 2

type Version = Int
type Levels  = [Level]

data Data = Data Version Levels deriving (Show, Read)

type Id       = Int
type Ids      = [Id]
type Entities = [Entity]
type Layers   = [Layer]
type Gravity  = Double

data Level = Level Entities Layers  deriving (Show, Read)
data Layer = Layer Id Entities Gravity deriving (Show, Read)

type Vector        = (Double, Double, Double)
type Box           = (Vector, Vector)
type Velocity      = Double
type Path          = [Vector]
type Bidirectional = Bool

data Animation = Animation Velocity Path Bidirectional deriving (Show, Read)

type Position            = Vector
type InitialPosition     = Position
type PositionOrAnimation = Either Position Animation
type Bound               = Box

data Entity = Player   Id InitialPosition
            | Platform Id PositionOrAnimation Bound
            | Star     Id Position deriving (Show, Read)
