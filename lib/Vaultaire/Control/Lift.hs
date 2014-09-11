{-# LANGUAGE
    RankNTypes
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , OverlappingInstances
  #-}
module Vaultaire.Control.Lift
     ( In(..) )
where

import           Control.Monad.Trans.Class

-- | Constraint a transformer stack @haystack@ to have a transformer @needle@ anywhere in the stack.
--   This relies on @OverlappingInstances@ to find the position of @needle@ in @haystack@,
--   we can use closed type families instead if needed.
class Monad haystack => In needle haystack where
  liftT :: (forall m. Monad m => needle m a) -> haystack a

instance (Monad m, Monad (t m)) => In t (t m) where
  liftT t = t

instance (In t s, MonadTrans t', Monad s, Monad (t' s)) => In t (t' s) where
  liftT t = lift (liftT t)
