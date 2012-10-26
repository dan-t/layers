
module Entity.Position where
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.Vect as V


currentPosition :: E.Entity -> V.Vect
currentPosition E.Player   {E.playerPosition   =       pos} = pos
currentPosition E.Star     {E.starPosition     =       pos} = pos
currentPosition E.Platform {E.platformPosition = Left  pos} = pos
currentPosition E.Platform {E.platformPosition = Right ani} = A.currentPosition ani


updateCurrentPosition :: E.Entity -> V.Vect -> E.Entity
updateCurrentPosition p@E.Player {} newPos = p {E.playerPosition = newPos}
updateCurrentPosition s@E.Star   {} newPos = s {E.starPosition   = newPos}

updateCurrentPosition p@E.Platform {E.platformPosition = Left _} newPos =
   p {E.platformPosition = Left newPos}

updateCurrentPosition p@E.Platform {E.platformPosition = Right ani} newPos =
   p {E.platformPosition = Right ani {A.currentPosition = newPos}}
