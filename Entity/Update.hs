
module Entity.Update where
import qualified Gamgine.Math.Vect as V
import Gamgine.Utils
import qualified GameData.Entity as E
import qualified GameData.Animation as A


data UpdateState = UpdateState {
   gravity :: Double
   } deriving Show


update :: UpdateState -> E.Entity -> E.Entity
update _ pf@E.Platform {E.platformPosition = (Right ani)} =
   pf {E.platformPosition = Right $ A.update ani}


update UpdateState {gravity = g} pl@E.Player {E.playerPosition = pos, E.playerVelocity = velo, E.playerWalkCycle = (step, angle)} =
   pl {E.playerPosition = pos + velo', E.playerVelocity = velo',
       E.playerOnBottom = False, E.playerWalkCycle = wc'}
   where
      velo'   = velo + V.v3 0 (-g) 0
      wc'     = (step' > maxStep ? 0 $ step', angle')
      angle'  = step' > maxStep ? negate angle $ angle
      step'   = step + 1
      maxStep = 20

update _ e = e
