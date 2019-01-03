{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FileData.ApplyToEntity where
import FileData.Data2
import Data.Monoid

class Monoid b => ApplyToEntity a b where
   apply :: (Entity -> b) -> a -> b

instance Monoid b => ApplyToEntity Entity b where
   apply f entity = f entity

instance Monoid b => ApplyToEntity Layer b where
   apply f (Layer entities _) = applyAE f entities

instance Monoid b => ApplyToEntity Level b where
   apply f (Level entities layers) =
      (applyAE f entities) `mappend` (applyAE f layers)

instance Monoid b => ApplyToEntity Data b where
   apply f (Data _ levels) = applyAE f levels

applyAE :: (Monoid b, ApplyToEntity a b) => (Entity -> b) -> [a] -> b
applyAE f apps = mconcat $ map (\app -> apply f app) apps
