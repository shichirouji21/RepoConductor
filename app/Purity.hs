{-# LANGUAGE CPP #-}

module Purity (getRepoName, filterMap, getPrint, getRepoColor, toLowerCase, parseTagLine, getMaxLength, safeInit, bold, replaceFirst) where

import qualified Data.Map as Map
import System.FilePath (splitPath, dropTrailingPathSeparator)
import Data.List (isInfixOf, isPrefixOf)
import Text.Printf (printf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Static

getRepoName :: FilePath -> String
getRepoName path = 
  let parts = splitPath (dropTrailingPathSeparator path)
  in  last parts

filterMap :: Map FilePath (String, Int, Int, Int, String) -> (Bool, String, String, String) -> Map FilePath (String, Int, Int, Int, String)
filterMap repos (fDirty, fName, fBranch, fTag) =
  Map.filterWithKey (\key value -> applyFilters key value (fDirty, fName, fBranch, fTag)) repos
  where
    applyFilters :: FilePath -> (String, Int, Int, Int, String) -> (Bool, String, String, String) -> Bool
    applyFilters key (branch, count1, count2, count3, tag) (fDirty, fName, fBranch, fTag) =
      let passesDirty = not fDirty || (count1 > 0 || count2 > 0 || count3 > 0)
          passesName = null fName || toLowerCase fName `isInfixOf` toLowerCase (getRepoName key)
          passesBranch = null fBranch || toLowerCase fBranch `isInfixOf` toLowerCase branch
          passesTag = null fTag || toLowerCase fTag `isInfixOf` toLowerCase tag
      in  passesDirty && passesName && passesBranch && passesTag

getPrint :: Int -> FilePath -> (String, Int, Int, Int, String) -> Maybe String
getPrint maxLength dir (branchVal, pullsVal, pushesVal, modifiedVal, tagVal) =
  let repoColor = getRepoColor pullsVal modifiedVal
      repoName = getRepoName dir
      pullColor = if pullsVal /= 0 then green else reset
      modifiedColor = if modifiedVal > 0 then red else reset
  in Just $ printf ("%s%-" ++ show (maxLength) ++ "s %-15s %s%-4s %s%-4s %s%-4s %s%-12s") 
                repoColor 
                repoName 
                (take 15 branchVal)
                pullColor 
                (if pullsVal /= 0
                 then show pullsVal
                 else if pullsVal == 0 && pushesVal == 0 && modifiedVal == 0
                 then "ok"
                 else mempty)
                reset
                (if pushesVal /= 0 then show pushesVal else mempty)
                modifiedColor 
                (if modifiedVal > 0 then show modifiedVal else mempty) 
                reset
                tagVal 

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

getMaxLength :: Map FilePath (String, Int, Int, Int, String) -> Int
getMaxLength repos = case Map.keys repos of
  []   -> 0
  keys -> maximum (map (length . getRepoName) keys)

safeInit :: String -> String
safeInit [] = []
safeInit xs = init xs

bold :: String -> String
bold s = "\x1b[1m" ++ s ++ "\x1b[0m"

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst old new str@(x:xs)
  | old `isPrefixOf` str = new ++ drop (length old) str
  | otherwise            = x : replaceFirst old new xs
replaceFirst _ _ [] = []
