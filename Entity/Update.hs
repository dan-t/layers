
module Entity.Update where
import qualified GameData.Entity as E
import qualified GameData.Animation as A

update :: E.Entity -> E.Entity
update pf@E.Platform {E.platformPosition = (Right ani)} =
   pf {E.platformPosition = Right $ A.update ani}

update e = e
