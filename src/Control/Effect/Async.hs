{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE KindSignatures #-}

module Control.Effect.Async
    ( Async(..), Future(..)
    , async, await
    ) where

import qualified Control.Concurrent.Async as Control.Concurrent
import Data.Kind (Type)
import Control.Algebra (Has, send)
import Data.Async (Future)

-- Syntax Definition for the Async effect.
-- This (GADT) data type defines how the async effect will be used by the caller
data Async (m :: Type -> Type) k where
    Async :: IO a -> Async m (Future a)
    Await :: Future a -> Async m a

-- Convenience functions that wrap the `send` function
-- That sends the Effect Syntax to the chosen Effect Handler
async :: (Has Async sig m) 
    => IO a -> m (Future a)
async = send . Async

await :: (Has Async sig m) 
    => Future a -> m a
await = send . Await