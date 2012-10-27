
module Entity.Position where
import qualified Data.List as L
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.Vect as V


currentPosition :: E.Entity -> V.Vect
currentPosition E.Player   {E.playerPosition   =       pos} = pos
currentPosition E.Star     {E.starPosition     =       pos} = pos
currentPosition E.Platform {E.platformPosition = Left  pos} = pos
currentPosition E.Platform {E.platformPosition = Right ani} = A.currentPosition ani


setCurrentPosition :: E.Entity -> V.Vect -> E.Entity
setCurrentPosition p@E.Player {} newPos = p {E.playerPosition = newPos}
setCurrentPosition s@E.Star   {} newPos = s {E.starPosition   = newPos}

setCurrentPosition p@E.Platform {E.platformPosition = Left _} newPos =
   p {E.platformPosition = Left newPos}

setCurrentPosition p@E.Platform {E.platformPosition = Right ani} newPos =
   p {E.platformPosition = Right ani {A.currentPosition = newPos}}


position :: E.Entity -> V.Vect
position E.Platform {E.platformPosition = Right ani} = L.head . A.path $ ani
position entity                                      = currentPosition entity


setPosition :: E.Entity -> V.Vect -> E.Entity
setPosition p@E.Platform {E.platformPosition = Right ani} newPos =
   let (basePt : path) = A.path ani
       diffPath        = L.map ((-) basePt) path
       path'           = L.map (newPos `plus`) diffPath
       in p {E.platformPosition = Right $ A.newAnimation (A.velocity ani)
                                                         (newPos : path')
                                                         (A.bidirectional ani)}
   where
      plus = (+)

setPosition entity newPos = setCurrentPosition entity newPos
