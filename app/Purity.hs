module Purity (getRepoName, filterMap, getPrint, getRepoColor, toLowerCase, parseTagLine, getMaxLength, safeInit) where

import qualified Data.Map as Map
import System.FilePath (dropTrailingPathSeparator, splitPath)
import Data.List (isInfixOf)
import Text.Printf (printf)
import Data.Char (toLower)
import Static
import Types (RepoFilters(..), RepoMap, RepoStatus(..), JobState(..), jobLabel)

getRepoName :: FilePath -> String
getRepoName path =
  let parts = splitPath (dropTrailingPathSeparator path)
  in  last parts

filterMap :: RepoMap -> RepoFilters -> RepoMap
filterMap repos repoFilters =
  Map.filterWithKey (\key status -> applyFilters key status repoFilters) repos
  where
    applyFilters :: FilePath -> RepoStatus -> RepoFilters -> Bool
    applyFilters key status filters =
      let passesDirty = not (filterDirty filters) || (statusPulls status > 0 || statusPushes status > 0 || statusModified status > 0)
          passesName = null (filterName filters) || toLowerCase (filterName filters) `isInfixOf` toLowerCase (getRepoName key)
          passesBranch = null (filterBranch filters) || toLowerCase (filterBranch filters) `isInfixOf` toLowerCase (statusBranch status)
          passesTag = null (filterTag filters) || toLowerCase (filterTag filters) `isInfixOf` toLowerCase (statusTags status)
      in  passesDirty && passesName && passesBranch && passesTag

getPrint :: Int -> FilePath -> RepoStatus -> String
getPrint maxLength dir status =
  let repoColor = getRepoColor (statusPulls status) (statusModified status)
      repoName = getRepoName dir
      pullColor = if statusPulls status /= 0 then green else reset
      modifiedColor = if statusModified status > 0 then red else reset
      (jobColor, jobText) = renderJob (statusJob status)
  in printf ("%s%-" ++ show maxLength ++ "s %-15s %s%-4s %s%-4s %s%-4s %s%-5s %s%-12s%s")
                 repoColor
                 repoName
                 (take 15 (statusBranch status))
                 pullColor
                 (if statusPulls status /= 0
                  then show (statusPulls status)
                  else if statusPulls status == 0 && statusPushes status == 0 && statusModified status == 0
                  then "ok"
                  else mempty)
                 reset
                 (if statusPushes status /= 0 then show (statusPushes status) else mempty)
                 modifiedColor
                 (if statusModified status > 0 then show (statusModified status) else mempty)
                 jobColor
                 jobText
                 reset
                 (statusTags status)
                 reset

renderJob :: JobState -> (String, String)
renderJob JobIdle      = (reset, "")
renderJob JobPending   = (yellow, jobLabel JobPending)
renderJob JobRunning   = (yellow, jobLabel JobRunning)
renderJob JobOk        = (green, jobLabel JobOk)
renderJob (JobErr _)   = (red, "err")

getRepoColor :: Int -> Int -> String
getRepoColor pullsVal modifiedVal
  | pullsVal > 0 && modifiedVal > 0 = yellow
  | pullsVal > 0                    = green
  | modifiedVal > 0                 = red
  | otherwise                       = reset

toLowerCase :: String -> String
toLowerCase = map toLower

parseTagLine :: String -> (FilePath, String)
parseTagLine line =
  case break (== '/') line of
    (key, _:rest) -> (key, dropWhile (== '\"') $ takeWhile (/= '\"') rest)
    _ -> (line, "core")

getMaxLength :: RepoMap -> Int
getMaxLength repos = case Map.keys repos of
  []   -> 0
  keys -> maximum (map (length . getRepoName) keys)

safeInit :: String -> String
safeInit [] = []
safeInit xs = init xs
