-- this module is in experimental state

{-# LANGUAGE OverloadedStrings #-}

module Taggy (readTags, addTagFiltered, removeTagFiltered, clearTagsFiltered) where

import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Map (Map)
import Data.List (intercalate)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import OS (getConfigPath)
import Purity (getRepoName)
import Types (RepoMap, RepoStatus(..))

type TagMap = Map FilePath [String]

readTags :: [FilePath] -> IO TagMap
readTags repoPaths = do
    tags <- getTagsFilePath
    tagsExists <- doesFileExist tags
    unless tagsExists $ createTags tags
    fileLines <- if tagsExists then fmap T.lines (TIO.readFile tags) else pure []
    let fileMap = migrateTags repoPaths $ Map.fromList $ map (parseLine . T.unpack) $ filter (not . T.null) fileLines
    let updatedMap = foldr ensureRepoPath fileMap repoPaths
    writeTagsFile updatedMap
    return $ Map.fromList [(path, Map.findWithDefault [] path updatedMap) | path <- repoPaths]

addTagFiltered :: String -> RepoMap -> RepoMap -> IO RepoMap
addTagFiltered tagName filteredRepos repoMap = do
    tags <- readTags (Map.keys repoMap)
    let updatedTags = Map.unionWith (\oldVals newVals -> if tagName `elem` oldVals then oldVals else oldVals ++ newVals)
                                      tags
                                      (Map.map (const [tagName]) filteredRepos)
    writeTagsFile updatedTags
    return $ applyTagsToRepoMap updatedTags repoMap

removeTagFiltered :: String -> RepoMap -> RepoMap -> IO RepoMap
removeTagFiltered tagName filteredRepos repoMap = do
    tags <- readTags (Map.keys repoMap)
    let updatedTags = foldr (Map.adjust (filter (/= tagName))) tags (Map.keys filteredRepos)
    writeTagsFile updatedTags
    return $ applyTagsToRepoMap updatedTags repoMap

clearTagsFiltered :: RepoMap -> RepoMap -> IO RepoMap
clearTagsFiltered filteredRepos repoMap = do
    tags <- readTags (Map.keys repoMap)
    let updatedTags = foldr (Map.adjust (const [])) tags (Map.keys filteredRepos)
    writeTagsFile updatedTags
    return $ applyTagsToRepoMap updatedTags repoMap

getTagsFilePath :: IO FilePath
getTagsFilePath = getConfigPath ("repoconductor" </> "tags.shi")

writeTagsFile :: TagMap -> IO ()
writeTagsFile tagMap = do
    tags <- getTagsFilePath
    let updatedLines = map (T.pack . formatLine) $ Map.toList tagMap
    TIO.writeFile tags (T.unlines updatedLines)

parseLine :: String -> (FilePath, [String])
parseLine line =
    case readMaybe line of
        Just parsed -> parsed
        Nothing     -> parseLegacyLine line

parseLegacyLine :: String -> (FilePath, [String])
parseLegacyLine line =
    let (key, rest) = span (/= '/') line
        values = case rest of
            '/' : vals -> filter (not . null) $ splitOn '/' vals
            _          -> []
    in (key, values)

formatLine :: (FilePath, [String]) -> String
formatLine = show

migrateTags :: [FilePath] -> TagMap -> TagMap
migrateTags repoPaths tagMap = Map.union tagsWithoutMigratedKeys migratedTags
  where
    repoNameCounts = Map.fromListWith (+) [(getRepoName path, 1 :: Int) | path <- repoPaths]
    migratedLegacyKeys = [name | (name, count) <- Map.toList repoNameCounts, count == 1, Map.member name tagMap]
    tagsWithoutMigratedKeys = foldr Map.delete tagMap migratedLegacyKeys
    migratedTags = Map.fromList
        [ (path, values)
        | path <- repoPaths
        , not (Map.member path tagMap)
        , Map.findWithDefault 0 (getRepoName path) repoNameCounts == 1
        , Just values <- [Map.lookup (getRepoName path) tagMap]
        ]

ensureRepoPath :: FilePath -> TagMap -> TagMap
ensureRepoPath path = Map.insertWith (\_ existing -> existing) path []

applyTagsToRepoMap :: TagMap -> RepoMap -> RepoMap
applyTagsToRepoMap tagMap = Map.mapWithKey (\filePath status ->
    status { statusTags = joinTags (Map.findWithDefault [] filePath tagMap) })

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr (\c acc -> if c == delimiter then [] : acc else (c : head acc) : tail acc) [[]]

joinTags :: [String] -> String
joinTags = intercalate "|"

createTags :: FilePath -> IO ()
createTags tags = do
    createDirectoryIfMissing True (takeDirectory tags)
    TIO.writeFile tags ""
