{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Effect.IndexerTestingFramework where
import Data.Kind (Type)
import Control.Algebra (Has, send)
import GHC.Base (Symbol)
import Data.Indexer (ClientArgs, ClientResult, ServerArgs, IndexerLog)
import qualified Data.Aeson

data Phantom :: Symbol -> Type where
    Phantom :: Phantom a

data FrameworkConfig (framework :: Symbol) = MkFrwkCfg
    { logsConfig :: FrameworkLogsConfig framework
    , serverConfig :: FrameworkServerConfig framework
    , clientConfig :: FrameworkClientConfig framework
    }
data family FrameworkLogsConfig   :: Symbol -> Type
data family FrameworkServerHandle :: Symbol -> Type
data family FrameworkServerConfig :: Symbol -> Type
data family FrameworkClientConfig :: Symbol -> Type
data IndexerTestingFramework (framework :: Symbol) (m :: Type -> Type) k where
    RunServer      
        :: ServerArgs -> FrameworkServerConfig framework
        -> IndexerTestingFramework framework m (FrameworkServerHandle framework)
    ShutdownServer 
        :: FrameworkServerHandle framework 
        -> IndexerTestingFramework framework m ()
    RunClient      
        :: ClientArgs -> FrameworkClientConfig framework
        -> IndexerTestingFramework framework m Data.Aeson.Value
    ReadLogs       
        :: FrameworkLogsConfig framework
        -> IndexerTestingFramework framework m [IndexerLog]
    Assert
        :: Phantom framework -> Bool
        -> IndexerTestingFramework framework m ()

runServer 
    :: Has (IndexerTestingFramework framework) sig m 
    => ServerArgs -> FrameworkServerConfig framework
    -> m (FrameworkServerHandle framework)
runServer args = send . RunServer args

shutdownServer
    :: Has (IndexerTestingFramework framework) sig m
    => FrameworkServerHandle framework -> m ()
shutdownServer = send . ShutdownServer

runClient
    :: Has (IndexerTestingFramework framework) sig m
    => ClientArgs -> FrameworkClientConfig framework -> m Data.Aeson.Value
runClient args = send . RunClient args

readLogs
    :: Has (IndexerTestingFramework framework) sig m
    => FrameworkLogsConfig framework -> m [IndexerLog]
readLogs = send . ReadLogs

assert
    :: Has (IndexerTestingFramework framework) sig m
    => Phantom framework -> Bool -> m ()
assert phantom = send . Assert phantom