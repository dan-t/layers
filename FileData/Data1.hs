{-# LANGUAGE TypeOperators #-}

module FileData.Data1 where

dataVersion :: Int
dataVersion = 1

type Tuple3d = (Double,Double,Double)
type Box     = (Tuple3d,Tuple3d)

data Moving = Moving {
   velocity      :: Tuple3d,
   path          :: [Tuple3d],
   bidirectional :: Bool
   } deriving (Show, Read)


data Platform = Platform {
   platformId :: Int,
   size       :: Box,
   position   :: Either Tuple3d Moving
   } deriving (Show, Read)

data Star = Star {
   starId       :: Int,
   starPosition :: Tuple3d
   } deriving (Show, Read)

data Layer = Layer {
   layerId   :: Int,
   gravity   :: Double,
   platforms :: [Platform]
   } deriving (Show, Read)

data Player = Player {
   playerId        :: Int,
   initialPosition :: Tuple3d
   } deriving (Show, Read)

data Level = Level {
   levelId          :: Int,
   player           :: Player,
   activeLayer      :: Layer,
   otherLayer       :: Layer,
   stars            :: [Star]
   } deriving (Show, Read)

data Data = Data {
   version :: Int,
   levels  :: [Level]
   } deriving (Show, Read)
