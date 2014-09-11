{-# LANGUAGE
      ScopedTypeVariables
    , ConstraintKinds
    , FlexibleContexts
    #-}

module Vaultaire.Control.Safe
     ( MonadSafeIO, safeLiftIO, bracketSafe, runSafeIO, runSafeIO' )
where

import           Control.Error.Util         (syncIO)
import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.Trans.Either (runEitherT)
import           Pipes
import qualified Pipes.Lift                 as P

-- | IO-capable monad that can be used with @safeLiftIO@ to bracket exceptions.
type MonadSafeIO m = (MonadIO m, MonadError SomeException m)

-- | Like @LiftIO@, but ensure that resources are cleaned up even if an IO exception occurs.
safeLiftIO :: MonadSafeIO m => IO a -> m a
safeLiftIO = either (throwError) (return) <=< runEitherT . syncIO

-- | Like @bracket@, but convert IO exceptions into errors so clean-up can be run.
bracketSafe :: MonadSafeIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketSafe start finish act = do
  a <- start
  catchError (do ret <- act a
                 _   <- finish a
                 return ret)
             (\(exc :: SomeException) -> finish a >> throwError exc)

-- orphaned instance of Error since we want to rethrow bracketed IO exceptions
-- should never be run, this is just to satify the Error constraint
-- should be removed when pipes is updated to use the new Except
instance Error SomeException where
  strMsg = error

-- | Runs the last layer of the transformer stack, which encapsulates exceptions as errors.
--   Rethrows those exceptions into the base monad.
runSafeIO :: MonadIO m
          => ListT (ErrorT SomeException m) x
          -> ListT m x
runSafeIO (Select p) = Select $ runSafeIO' p

runSafeIO' :: MonadIO m
           => Producer x (ErrorT SomeException m) r
           -> Producer x m r
runSafeIO' p = P.runErrorP p >>= either throw return
