
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

data Level = Level Id Entities Layers  deriving (Show, Read)
data Layer = Layer Id Entities Gravity deriving (Show, Read)

type Tuple3d       = (Double, Double, Double)
type Box           = (Tuple3d, Tuple3d)
type Velocity      = Tuple3d
type Path          = [Tuple3d]
type Bidirectional = Bool

data Animation = Animation Velocity Path Bidirectional deriving (Show, Read)

type Position            = Tuple3d
type InitialPosition     = Position
type PositionOrAnimation = Either Position Animation
type Size                = Box

data Entity = Player   Id InitialPosition     Size
            | Platform Id PositionOrAnimation Size
            | Star     Id Position            Size
            | Enemy    Id Animation           Size deriving (Show, Read)

entityId :: Entity -> Id
entityId (Player   id _ _) = id
entityId (Platform id _ _) = id
entityId (Star     id _ _) = id
entityId (Enemy    id _ _) = id