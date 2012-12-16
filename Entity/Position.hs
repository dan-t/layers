
module Entity.Position where
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.Vect as V


currentPosition :: E.Entity -> V.Vect
currentPosition E.Player   {E.playerPosition   =       pos} = pos
currentPosition E.Enemy    {E.enemyPosition    = Left  pos} = pos
currentPosition E.Enemy    {E.enemyPosition    = Right ani} = A.currentPosition ani
currentPosition E.Star     {E.starPosition     =       pos} = pos
currentPosition E.Platform {E.platformPosition = Left  pos} = pos
currentPosition E.Platform {E.platformPosition = Right ani} = A.currentPosition ani


setCurrentPosition :: E.Entity -> V.Vect -> E.Entity
setCurrentPosition p@E.Player {} newPos = p {E.playerPosition = newPos}

setCurrentPosition e@E.Enemy {E.enemyPosition = Left _} newPos =
   e {E.enemyPosition = Left newPos}

setCurrentPosition e@E.Enemy {E.enemyPosition = Right ani} newPos =
   e {E.enemyPosition = Right ani {A.currentPosition = newPos}}

setCurrentPosition s@E.Star   {} newPos = s {E.starPosition   = newPos}

setCurrentPosition p@E.Platform {E.platformPosition = Left _} newPos =
   p {E.platformPosition = Left newPos}

setCurrentPosition p@E.Platform {E.platformPosition = Right ani} newPos =
   p {E.platformPosition = Right ani {A.currentPosition = newPos}}


position :: E.Entity -> V.Vect
position E.Enemy    {E.enemyPosition    = Right ani} = A.basePosition ani
position E.Platform {E.platformPosition = Right ani} = A.basePosition ani
position entity                                      = currentPosition entity


setPosition :: E.Entity -> V.Vect -> E.Entity
setPosition p@E.Player {} newPos = p {E.playerInitialPos = newPos, E.playerPosition = newPos}

setPosition e@E.Enemy {E.enemyPosition = Right ani} newPos =
   e {E.enemyPosition = Right $ A.setBasePosition ani newPos}

setPosition p@E.Platform {E.platformPosition = Right ani} newPos =
   p {E.platformPosition = Right $ A.setBasePosition ani newPos}

setPosition entity newPos = setCurrentPosition entity newPos
