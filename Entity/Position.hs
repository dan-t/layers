
module Entity.Position where
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.Vect as V


currentPosition :: E.Entity -> V.Vect
currentPosition E.Player   {E.playerPosition   =       pos} = pos
currentPosition E.Star     {E.starPosition     =       pos} = pos
currentPosition E.Platform {E.platformPosition = Left  pos} = pos
currentPosition E.Platform {E.platformPosition = Right ani} = A.currentPosition ani


updatePosition :: E.Entity -> V.Vect -> E.Entity
updatePosition p@E.Player {} newPos = p {E.playerPosition = newPos}
updatePosition s@E.Star   {} newPos = s {E.starPosition   = newPos}

updatePosition p@E.Platform {E.platformPosition = Left _} newPos = 
   p {E.platformPosition = Left newPos}

updatePosition p@E.Platform {E.platformPosition = Right ani} newPos =
   p {E.platformPosition = Right ani {A.currentPosition = newPos}}
