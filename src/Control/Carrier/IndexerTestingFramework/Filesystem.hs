{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Carrier.IndexerTestingFramework.Filesystem where
import Control.Effect.IndexerTestingFramework (IndexerTestingFramework (RunServer, ShutdownServer, RunClient, ReadLogs, Assert), FrameworkServerHandle, FrameworkServerConfig, FrameworkClientConfig, FrameworkLogsConfig)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Algebra (Algebra (alg), (:+:) (L, R), Has)
import Control.Effect.Async (Async)
import System.Process (ProcessHandle, shell, createProcess, cleanupProcess, proc)
import Data.Indexer (ServerArgs(..), ClientArgs (..), makeArguments, ClientResult (Failure))
import GHC.IO.Handle
import qualified Control.Effect.IndexerTestingFramework as Data
import qualified Data.Indexer as Data.IndexerTestingFramework
newtype FilesystemIndexerFramework m a = FilesystemIndexerFrameworkIO
    { runFilesystemIndexerFramework :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype instance FrameworkServerHandle "filesystem" = FilesystemServerH
    { unServerHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) }
newtype instance FrameworkServerConfig "filesystem" = FilesystemServerC
    { indexerBinaryPathServer :: FilePath }
newtype instance FrameworkClientConfig "filesystem" = FilesystemClientC
    { indexerBinaryPathClient :: FilePath }
newtype instance FrameworkLogsConfig "filesystem" = FilesystemLogsC
    { logsPath :: FilePath }

instance
    ( Algebra sig m
    , MonadIO m
    ) => Algebra (IndexerTestingFramework "filesystem" :+: sig) (FilesystemIndexerFramework m) where
    alg hdl sig ctx = case sig of
        L (RunServer serverArgs frameworkConfig) -> (<$ ctx) <$> do
            let indexerServerProcess = 
                    proc (indexerBinaryPathServer frameworkConfig) (serverProcessData serverArgs)
            indexerProcessHandle <- liftIO 
                $ System.Process.createProcess indexerServerProcess
            return $ FilesystemServerH indexerProcessHandle

        L (ShutdownServer serverHandle) -> (<$ ctx) <$> do
            liftIO $ System.Process.cleanupProcess (unServerHandle serverHandle)

        L (RunClient clientArgs frameworkConfig) -> (<$ ctx) <$> do
            let indexerClientProcess = 
                    proc (indexerBinaryPathClient frameworkConfig) (clientProcessData clientArgs)
            (m_stdin, m_stdout, m_stderr, handle) <- liftIO
                $ System.Process.createProcess indexerClientProcess
            case m_stdout of
                Nothing -> return Data.IndexerTestingFramework.Failure
                Just stdout -> do
                    clientOutput <- liftIO
                        $ hGetContents stdout
                    undefined


        L (ReadLogs frameworkConfig) -> undefined
        L (Assert _ assertion) -> undefined
        R other -> FilesystemIndexerFrameworkIO (alg (runFilesystemIndexerFramework . hdl) other ctx)

serverProcessData :: ServerArgs -> [String]
serverProcessData args = "server"
    : case args of
        Config configPath -> 
            "config -p " 
            : [show configPath]
        Cli cliArgs -> 
            "cli" 
            : makeArguments cliArgs

clientProcessData :: ClientArgs -> [String]
clientProcessData args = "client"
    : case args of
        SummaryCommand -> 
            ["summary"]
        LedgerCommand -> 
            ["best-ledger"]
        BestChainCommand maxLength -> 
            ["best-chain -n " ++ show maxLength]
        AccountCommand publicKey ->
            ["account --public-key " ++ publicKey]