module Types (RepoFilters(..), RepoMap, RepoStatus(..)) where

import Data.Map (Map)

data RepoStatus = RepoStatus {
  statusBranch   :: String,
  statusPulls    :: Int,
  statusPushes   :: Int,
  statusModified :: Int,
  statusTags     :: String
} deriving (Eq, Show)

type RepoMap = Map FilePath RepoStatus

data RepoFilters = RepoFilters {
  filterDirty  :: Bool,
  filterName   :: String,
  filterBranch :: String,
  filterTag    :: String
} deriving (Eq, Show)
