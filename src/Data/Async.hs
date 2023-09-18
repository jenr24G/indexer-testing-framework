{-# LANGUAGE LambdaCase #-}
module Data.Async
    ( Future(..)
    , OpaqueHList(..)
    , removeFuture, awaitFutures, cancelFutures
    ) where

import Data.Kind (Type)
import Control.Concurrent (ThreadId, throwTo)
import Control.Exception (SomeException, BlockedIndefinitelyOnSTM (..), catch)
import Control.Concurrent.STM (STM, atomically, throwSTM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (void)
import Control.Concurrent.Async

data Future a = Future
    { threadID :: {-# UNPACK #-} !ThreadId
    , joinHandle :: STM (Either SomeException a)
    } 

data OpaqueHList (f :: Type -> Type) where
    HNil :: forall (f :: Type -> Type). OpaqueHList f
    HCons
        :: forall (f :: Type -> Type) (a :: Type). 
        f a -> OpaqueHList f -> OpaqueHList f

removeFuture :: ThreadId -> OpaqueHList Future -> OpaqueHList Future
removeFuture removeThreadID = \case
  HNil -> HNil
  HCons future futures -> if threadID future == removeThreadID
    then removeFuture removeThreadID futures
    else HCons future $ 
      removeFuture removeThreadID futures

awaitFutures :: MonadIO m => OpaqueHList Future -> m ()
awaitFutures HNil = pure ()
awaitFutures (HCons (Future _ joinHandle) futures) = do
  let tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f
  void . liftIO . tryAgain . atomically $
    joinHandle >>= either throwSTM return
  awaitFutures futures

cancelFutures :: MonadIO m => OpaqueHList Future -> m ()
cancelFutures HNil = pure ()
cancelFutures (HCons future futures) = do
    let tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f
    liftIO $ 
        throwTo (threadID future) AsyncCancelled 
        <* (tryAgain . atomically . joinHandle) future
    cancelFutures futures