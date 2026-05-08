module Types (RepoFilters(..), RepoMap, RepoStatus(..), JobState(..), jobLabel, ConductorOptions(..), defaultOptions) where

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

data ConductorOptions = ConductorOptions {
  optionPath             :: Maybe FilePath,
  optionRecurse          :: Bool,
  optionJobs             :: Maybe Int,
  optionStartupRefresh   :: Bool,
  optionDebug            :: Bool
} deriving (Eq, Show)

defaultOptions :: ConductorOptions
defaultOptions = ConductorOptions {
  optionPath           = Nothing,
  optionRecurse        = True,
  optionJobs           = Nothing,
  optionStartupRefresh = True,
  optionDebug          = False
}

