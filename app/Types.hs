module Types (RepoFilters(..), RepoMap, RepoStatus(..), JobState(..), jobLabel) where

import Data.Map (Map)

data JobState
  = JobIdle
  | JobPending
  | JobRunning
  | JobOk
  | JobErr String
  deriving (Eq, Show)

jobLabel :: JobState -> String
jobLabel JobIdle      = ""
jobLabel JobPending   = "wait"
jobLabel JobRunning   = "run"
jobLabel JobOk        = "ok"
jobLabel (JobErr _)   = "err"

data RepoStatus = RepoStatus {
  statusBranch   :: String,
  statusPulls    :: Int,
  statusPushes   :: Int,
  statusModified :: Int,
  statusTags     :: String,
  statusJob      :: JobState
} deriving (Eq, Show)

type RepoMap = Map FilePath RepoStatus

data RepoFilters = RepoFilters {
  filterDirty  :: Bool,
  filterName   :: String,
  filterBranch :: String,
  filterTag    :: String
} deriving (Eq, Show)
