{-# LANGUAGE FlexibleInstances #-}

module GameData.Entity where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Math.BoxTree as BT
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import qualified FileData.Data2 as FD
import qualified GameData.Animation as A
IMPORT_LENS

-- | for classification of a box inside the box tree
data Position = Top | Bottom | Left | Right | Center | Whatever deriving (Show, Eq)

-- | the bound of an entity
type Bound = BT.BoxTree Position

-- | the scope of the entity
data Scope = LevelScope         -- ^ entity is part of all layers
           | ActiveLayerScope   -- ^ entity is part of the currently active layer
           | InactiveLayerScope -- ^ entity is part of a currently inactive layer
           deriving (Show, Eq)

-- | in which direction the player is moving
data Movement = ToTheLeft | ToTheRight | AtRest deriving (Show, Eq)

-- | the position of a platform
type PositionOrAnimation = Either V.Vect A.Animation

-- | the game entites
data Entity = Player {playerId         :: Int,
                      playerInitialPos :: V.Vect,
                      playerPosition   :: V.Vect,
                      playerVelocity   :: V.Vect,
                      playerOnBottom   :: Bool,
                      playerBound      :: Bound,
                      playerWalkCycle  :: (Int, Double)}

            | Star {starId        :: Int,
                    starPosition  :: V.Vect,
                    starBound     :: B.Box,
                    starCollected :: Bool}

            | Platform {platformId       :: Int,
                        platformPosition :: PositionOrAnimation,
                        platformBound    :: B.Box}

            deriving Show

LENS(playerId)
LENS(playerInitialPos)
LENS(playerPosition)
LENS(playerVelocity)
LENS(playerOnBottom)
LENS(playerBound)
LENS(playerWalkCycle)

LENS(starId)
LENS(starPosition)
LENS(starBound)
LENS(starCollected)

LENS(platformId)
LENS(platformPosition)
LENS(platformBound)


class ApplyToEntity a where
   eMap :: (Entity -> Entity) -> a -> a


instance ApplyToEntity [Entity] where
   eMap f es = L.map f es


isPlayer :: Entity -> Bool
isPlayer Player {} = True
isPlayer _         = False

isStar :: Entity -> Bool
isStar Star {} = True
isStar _       = False

isPlatform :: Entity -> Bool
isPlatform Platform {} = True
isPlatform _           = False
