
module Entity.Update where
import qualified Gamgine.Math.Vect as V
import Gamgine.Control ((?))
import qualified GameData.Entity as E
import qualified GameData.Animation as A


data UpdateState = UpdateState {
   gravity :: Double
   } deriving Show


update :: UpdateState -> E.Entity -> E.Entity
update _ pf@E.Platform {E.platformPosition = (Right ani)} =
   pf {E.platformPosition = Right $ A.update ani}


update _ e@E.Enemy {E.enemyPosition = Right ani, E.enemyWalkCycle = wc} =
   e {E.enemyPosition = Right $ A.update ani, E.enemyWalkCycle = updateWalkCycle wc}


update UpdateState {gravity = g} pl@E.Player {E.playerPosition = pos, E.playerVelocity = velo, E.playerWalkCycle = wc} =
   pl {E.playerPosition = pos + velo', E.playerVelocity = velo',
       E.playerOnBottom = False, E.playerWalkCycle = wc'}
   where
      velo' = velo + V.v3 0 (-g) 0
      wc'   = updateWalkCycle wc


update _ e = e


updateWalkCycle :: (Int, Double) -> (Int, Double)
updateWalkCycle (step, angle) = wc'
   where
      wc'     = (step' > maxStep ? 0 $ step', angle')
      angle'  = step' > maxStep ? negate angle $ angle
      step'   = step + 1
      maxStep = 20
