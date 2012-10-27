
module Entity.Id where
import qualified GameData.Entity as E

invalidId :: Int
invalidId = -1


entityId :: E.Entity -> Int
entityId E.Player   {E.playerId   = id} = id
entityId E.Star     {E.starId     = id} = id
entityId E.Platform {E.platformId = id} = id


setEntityId :: E.Entity -> Int -> E.Entity
setEntityId p@E.Player   {} id = p {E.playerId   = id}
setEntityId s@E.Star     {} id = s {E.starId     = id}
setEntityId p@E.Platform {} id = p {E.platformId = id}
