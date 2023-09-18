{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Indexer 
    ( ClientArgs(..), ClientResult(..)
    , SummaryResultData(..), WitnessTreeSummary(..), DBStats(..), BestChainResultData(..), AccountResultData(..)
    , ServerArgs(..), makeArguments
    , IndexerLog
    ) where
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import GHC.Generics (Generic)

data ClientArgs
    = SummaryCommand
    | LedgerCommand
    | BestChainCommand Int
    | AccountCommand String

data ClientResult
    = SummaryResult SummaryResultData
    | LedgerResult String
    | BestChainResult [BestChainResultData]
    | AccountResult AccountResultData
    | Failure


data SummaryResultData = SummaryResultD
    { uptime :: Int
    , dateTime :: Int
    , witnessTree :: WitnessTreeSummary
    , dbStats :: DBStats
    }


data WitnessTreeSummary = WTSummary
    { bestTipLength      :: Int
    , bestTipHash        :: String
    , canonicalTipLength :: Int
    , canonicalTipHash   :: String
    , rootLength         :: Int
    , rootHash           :: String
    , numLeaves          :: Int
    , numDangling        :: Int
    , maxDanglingHeight  :: Int
    , maxDanglingLength  :: Int
    , witnessTree        :: Maybe String
    }

data DBStats = DBStats
    { memory    :: Int
    , uptime    :: String
    , cumWrites :: String
    , cumWal    :: String
    , cumStall  :: String
    , intWrites :: String
    , intWal    :: String
    , intStall  :: String
    }

data BestChainResultData = BestChainResultD
    { len       :: Int
    , stateHash :: String
    }

data AccountResultData = AccountResultD
    { publicKey :: String
    , balance   :: Int
    , nonce     :: String
    , delegate  :: Maybe String
    }

data ServerArgs
    = Config FilePath
    | Cli ServerCli

data LevelFilter
    = Info
    | Debug
    | Error
    | Warn
    | Trace
instance Show LevelFilter where
  show = \case
    Info -> "info"
    Debug -> "debug"
    Error -> "error"
    Warn -> "warn"
    Trace -> "trace"


data ServerCli = MkServerCli
    { ledger :: FilePath
    , nonGenesisLedger :: Bool
    , rootHash :: String
    , startupDir :: FilePath
    , watchDir :: FilePath
    , databaseDir :: FilePath
    , logDir :: FilePath
    , keepNonCanonicalBlocks :: Bool
    , logLevel :: LevelFilter
    , logLevelStdout :: LevelFilter
    , pruneInterval :: Int
    , canonicalUpdateThreshold :: Int
    , snapshotPath :: Maybe FilePath
    }

data IndexerLog

makeArguments :: ServerCli -> [String]
makeArguments 
    ( MkServerCli 
        ledger 
        nonGenesisLedger 
        rootHash 
        startupDir 
        watchDir 
        databaseDir 
        logDir 
        keepNonCanonicalBlocks 
        logLevel
        logLevelStdout
        pruneInterval
        canonicalUpdateThreshold
        snapshotPath
    ) =
    [ "-l " ++ show ledger
    , "-n " ++ show nonGenesisLedger
    , "--root-hash " ++ rootHash
    , "-s " ++ show startupDir
    , "-w " ++ show watchDir
    , "-d " ++ show databaseDir
    , "-l " ++ show logDir 
    , "--log-level " ++ show logLevel
    , "--log-level-stdout " ++ show logLevelStdout
    , "-p " ++ show pruneInterval
    , "-c " ++ show canonicalUpdateThreshold 
    ] ++ (["-k" | keepNonCanonicalBlocks]) ++ (case snapshotPath of Nothing -> []; Just path -> [ "--snapshot-path " ++ show path])