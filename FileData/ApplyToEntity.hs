
module FileData.ApplyToEntity where
import FileData.Data2
import Data.Monoid

class Monoid b => ApplyToEntity a b where
   apply :: (Entity -> b) -> a -> b

instance Monoid b => ApplyToEntity Entity b where
   apply f entity = f entity

instance Monoid b => ApplyToEntity Layer b where
   apply f (Layer _ entities _) = apply_ f entities

instance Monoid b => ApplyToEntity Level b where
   apply f (Level _ entities layers) =
      (apply_ f entities) `mappend` (apply_ f layers)

instance Monoid b => ApplyToEntity Data b where
   apply f (Data _ levels) = apply_ f levels

apply_ :: (Monoid b, ApplyToEntity a b) => (Entity -> b) -> [a] -> b
apply_ f apps = mconcat $ map (\app -> apply f app) apps
