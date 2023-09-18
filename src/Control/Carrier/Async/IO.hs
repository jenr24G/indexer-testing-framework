{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Control.Carrier.Async.IO where
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Effect.Async (Async(..))
import Control.Algebra (Has, Algebra (alg), (:+:) (L, R))
import Control.Effect.State (State, get, put)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, readTMVar)
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (atomically, throwSTM)
import Control.Carrier.State.Strict (runState, StateC)
import Control.Exception (BlockedIndefinitelyOnSTM(..), catch)
import Data.Async (Future(..), OpaqueHList(..), removeFuture, awaitFutures, cancelFutures)

newtype AsyncIOCarrier m a = AsyncIO
    { runAsyncIO :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance
    ( Has (State (OpaqueHList Future)) sig m
    , MonadIO m
    , Algebra sig m
    ) => Algebra (Async :+: sig) (AsyncIOCarrier m) where
        alg hdl sig ctx = case sig of
            L (Async action) -> (<$ ctx) <$> do
                var      <- liftIO newEmptyTMVarIO
                threadID <- liftIO $ forkFinally action 
                  (atomically . putTMVar var)
                let future = Future threadID (readTMVar var)
                get >>= \futures -> put $ HCons future futures
                return future

            L (Await (Future threadID joinHandle)) -> (<$ ctx) <$> do
                let tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f
                get >>= \futures -> let 
                  awaitedFutures = removeFuture threadID futures 
                  in put awaitedFutures
                liftIO $ (tryAgain . atomically) 
                  (joinHandle >>= either throwSTM return)

            R other -> AsyncIO (alg (runAsyncIO . hdl) other ctx)

asyncIO 
  :: MonadIO m 
  => AsyncIOCarrier (StateC (OpaqueHList Future) m) a
  -> m a
asyncIO action = do
  (unawaitedFutures, result) <- runState HNil . runAsyncIO $ action
  awaitFutures unawaitedFutures
  return result

asyncIOCancel
  :: MonadIO m 
  => AsyncIOCarrier (StateC (OpaqueHList Future) m) a
  -> m a
asyncIOCancel action = do
  (unawaitedFutures, result) <- runState HNil . runAsyncIO $ action
  cancelFutures unawaitedFutures
  return result