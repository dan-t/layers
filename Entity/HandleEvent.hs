
module Entity.HandleEvent where
import Gamgine.Math.Vect as V
import qualified Event as EV
import qualified GameData.Entity as E
import qualified GameData.Player as PL


handleEvent :: EV.EntityEvent -> E.Entity -> E.Entity
handleEvent (EV.PlayerStartsMoving movement) player@E.Player {E.playerVelocity = velo} =
   player {E.playerVelocity = velo'}
   where
      velo' = velo + case movement of
                          E.ToTheLeft  -> V.v3 (-PL.playerVelocity) 0 0
                          E.ToTheRight -> V.v3 PL.playerVelocity 0 0
                          E.AtRest     -> V.v3 0 0 0

handleEvent (EV.PlayerStopsMoving movement) player@E.Player {E.playerVelocity = velo} =
   player {E.playerVelocity = velo'}
   where
      velo' = velo + case movement of
                          E.ToTheLeft  -> V.v3 PL.playerVelocity 0 0
                          E.ToTheRight -> V.v3 (-PL.playerVelocity) 0 0
                          E.AtRest     -> V.v3 0 0 0


handleEvent _ entity = entity
