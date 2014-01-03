
module Entity.Velocity where
import qualified GameData.Entity as E
import qualified GameData.Animation as A
import qualified Gamgine.Math.Vect as V


velocity :: E.Entity -> V.Vect
velocity E.Player   {E.playerVelocity   = v        } = v
velocity E.Enemy    {E.enemyPosition    = Left    _} = V.v3 0 0 0
velocity E.Enemy    {E.enemyPosition    = Right ani} = A.currentVelocity ani
velocity E.Star     {                              } = V.v3 0 0 0
velocity E.Platform {E.platformPosition = Left    _} = V.v3 0 0 0
velocity E.Platform {E.platformPosition = Right ani} = A.currentVelocity ani
