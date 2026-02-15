{-# LANGUAGE DeriveGeneric #-}
module Sabela.Server.Types where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TChan)
import Data.Aeson (ToJSON(..), FromJSON, object, (.=))
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Sabela.Session (Session)

-- ── Cell / Notebook ──────────────────────────────────────────────

data Cell = Cell
  { cellId     :: Int
  , cellType   :: CellType
  , cellSource :: Text
  , cellOutput :: Maybe Text
  , cellError  :: Maybe Text
  , cellMime   :: Text
  , cellDirty  :: Bool
  } deriving (Show, Eq, Generic)

data CellType = CodeCell | ProseCell
  deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell
instance ToJSON CellType
instance FromJSON CellType

data Notebook = Notebook
  { nbTitle :: Text
  , nbCells :: [Cell]
  } deriving (Show, Eq, Generic)

instance ToJSON Notebook
instance FromJSON Notebook

-- ── Requests ─────────────────────────────────────────────────────

data UpdateCell = UpdateCell
  { ucSource :: Text
  } deriving (Show, Generic)

instance ToJSON UpdateCell
instance FromJSON UpdateCell

data InsertCell = InsertCell
  { icAfter  :: Int
  , icType   :: CellType
  , icSource :: Text
  } deriving (Show, Generic)

instance ToJSON InsertCell
instance FromJSON InsertCell

data LoadRequest = LoadRequest
  { lrPath :: FilePath
  } deriving (Show, Generic)

instance ToJSON LoadRequest
instance FromJSON LoadRequest

data SaveRequest = SaveRequest
  { srPath :: Maybe FilePath     -- ^ Nothing = save to nbTitle
  } deriving (Show, Generic)

instance ToJSON SaveRequest
instance FromJSON SaveRequest

data CreateFileRequest = CreateFileRequest
  { cfPath    :: Text            -- ^ relative path
  , cfContent :: Text
  , cfIsDir   :: Bool
  } deriving (Show, Generic)

instance ToJSON CreateFileRequest
instance FromJSON CreateFileRequest

data WriteFileRequest = WriteFileRequest
  { wfPath    :: Text
  , wfContent :: Text
  } deriving (Show, Generic)

instance ToJSON WriteFileRequest
instance FromJSON WriteFileRequest

data CompleteRequest = CompleteRequest
  { crPrefix :: Text
  } deriving (Show, Generic)

instance ToJSON CompleteRequest
instance FromJSON CompleteRequest

data InfoRequest = InfoRequest
  { irName :: Text
  } deriving (Show, Generic)

instance ToJSON InfoRequest
instance FromJSON InfoRequest

-- ── Results ──────────────────────────────────────────────────────

data RunResult = RunResult
  { rrCellId :: Int
  , rrOutput :: Maybe Text
  , rrError  :: Maybe Text
  , rrMime   :: Text
  } deriving (Show, Generic)

instance ToJSON RunResult
instance FromJSON RunResult

data RunAllResult = RunAllResult
  { rarResults :: [RunResult]
  } deriving (Show, Generic)

instance ToJSON RunAllResult
instance FromJSON RunAllResult

data CompleteResult = CompleteResult
  { crCompletions :: [Text]
  } deriving (Show, Generic)

instance ToJSON CompleteResult
instance FromJSON CompleteResult

data InfoResult = InfoResult
  { irText :: Text
  } deriving (Show, Generic)

instance ToJSON InfoResult
instance FromJSON InfoResult

-- ── File explorer ────────────────────────────────────────────────

data FileEntry = FileEntry
  { feName   :: Text
  , fePath   :: Text
  , feIsDir  :: Bool
  } deriving (Show, Generic)

instance ToJSON FileEntry
instance FromJSON FileEntry

-- ── Examples ─────────────────────────────────────────────────────

data Example = Example
  { exTitle    :: Text
  , exDesc     :: Text
  , exCategory :: Text
  , exCode     :: Text
  } deriving (Show, Generic)

instance ToJSON Example
instance FromJSON Example

-- ── Structured errors ────────────────────────────────────────────

data CellError = CellError
  { ceLine    :: Maybe Int       -- ^ 1-based line within cell
  , ceCol     :: Maybe Int
  , ceMessage :: Text
  } deriving (Show, Generic)

instance ToJSON CellError
instance FromJSON CellError

-- ── SSE Events ───────────────────────────────────────────────────

data NotebookEvent
  = EvCellUpdating  Int
  | EvCellResult    Int (Maybe Text) (Maybe Text) Text [CellError]
  | EvExecutionDone
  | EvSessionStatus Text
  deriving (Show)

instance ToJSON NotebookEvent where
  toJSON (EvCellUpdating cid) =
    object ["type" .= ("cellUpdating" :: Text), "cellId" .= cid]
  toJSON (EvCellResult cid out err mime errs) =
    object ["type" .= ("cellResult" :: Text), "cellId" .= cid,
            "output" .= out, "error" .= err, "mime" .= mime,
            "errors" .= errs]
  toJSON EvExecutionDone =
    object ["type" .= ("executionDone" :: Text)]
  toJSON (EvSessionStatus msg) =
    object ["type" .= ("sessionStatus" :: Text), "message" .= msg]

-- ── App State ────────────────────────────────────────────────────

data AppState = AppState
  { stNotebook      :: MVar Notebook
  , stSession       :: MVar (Maybe Session)
  , stTmpDir        :: FilePath
  , stWorkDir       :: FilePath
  , stEnvFile       :: IORef (Maybe FilePath)
  , stNextId        :: IORef Int
  , stInstalledDeps :: IORef (Set Text)
  , stInstalledExts :: IORef (Set Text)
  , stBroadcast     :: TChan NotebookEvent
  , stGeneration    :: IORef Int
  , stDebounceRef   :: MVar (Maybe (Int, Set Int))  -- ^ (generation, edited cell IDs)
  }
