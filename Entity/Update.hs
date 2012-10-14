
module Entity.Update where
import qualified Gamgine.Math.Vect as V
import qualified GameData.Entity as E
import qualified GameData.Animation as A


data UpdateState = UpdateState {
   gravity :: Double
   } deriving Show


update :: UpdateState -> E.Entity -> E.Entity
update _ pf@E.Platform {E.platformPosition = (Right ani)} =
   pf {E.platformPosition = Right $ A.update ani}


update UpdateState {gravity = g} pl@E.Player {E.playerPosition = pos, E.playerVelocity = velo} =
   pl {E.playerPosition = pos + velo', E.playerVelocity = velo', E.playerOnBottom = False}
   where
      velo' = velo + V.v3 0 (-g) 0

update _ e = e
