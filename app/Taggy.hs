-- this module is in experimental state

{-# LANGUAGE OverloadedStrings #-}

module Taggy (readTags, addTagFiltered, removeTagFiltered, clearTagsFiltered) where

import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Map (Map)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intercalate)
import OS (getConfigPath)
import Purity (getRepoName)

readTags :: [String] -> IO (Map String [String])
readTags keys = do
    tags <- getTagsFilePath
    tagsExists <- doesFileExist tags
    unless tagsExists $ createTags tags
    fileLines <- if tagsExists then fmap T.lines (TIO.readFile tags) else pure []
    let fileMap = Map.fromList $ map (parseLine . T.unpack) fileLines
    let newMap = Map.fromList [(key, Map.findWithDefault [] key fileMap) | key <- keys]
    writeTagsFile newMap
    return newMap

addTagFiltered :: String -> Map FilePath (String, Int, Int, Int, String) -> Map FilePath (String, Int, Int, Int, String) -> IO (Map FilePath (String, Int, Int, Int, String))
addTagFiltered tagName filteredRepos repoMap = do
    tags <- readTags (map getRepoName (Map.keys repoMap))
    let updatedTags = Map.unionWith (\oldVals newVals -> if tagName `elem` oldVals then oldVals else oldVals ++ newVals)
                                     tags
                                     (Map.map (const [tagName]) $ Map.mapKeys getRepoName filteredRepos)
    writeTagsFile updatedTags
    let updatedRepoMap = Map.mapWithKey (\filePath (a, b, c, d, _) ->
            let repoName = getRepoName filePath
                tagString = joinTags (Map.findWithDefault [] repoName updatedTags)
            in (a, b, c, d, tagString)) repoMap
    return updatedRepoMap

removeTagFiltered :: String -> Map FilePath (String, Int, Int, Int, String) -> Map FilePath (String, Int, Int, Int, String) -> IO (Map FilePath (String, Int, Int, Int, String))
removeTagFiltered tagName filteredRepos repoMap = do
    tags <- readTags (map getRepoName (Map.keys repoMap))
    let updatedTags = Map.unionWith (\oldVals newVals -> filter (/= tagName) oldVals)
                                     tags
                                     (Map.map (const []) $ Map.mapKeys getRepoName filteredRepos)
    writeTagsFile updatedTags
    let updatedRepoMap = Map.mapWithKey (\filePath (a, b, c, d, _) ->
            let repoName = getRepoName filePath
                tagString = joinTags (Map.findWithDefault [] repoName updatedTags)
            in (a, b, c, d, tagString)) repoMap
    return updatedRepoMap

clearTagsFiltered :: Map FilePath (String, Int, Int, Int, String) -> Map FilePath (String, Int, Int, Int, String) -> IO (Map FilePath (String, Int, Int, Int, String))
clearTagsFiltered filteredRepos repoMap = do
    tags <- readTags (map getRepoName (Map.keys repoMap))
    let updatedTags = Map.unionWith (\_ _ -> []) tags (Map.map (const []) $ Map.mapKeys getRepoName filteredRepos)
    writeTagsFile updatedTags
    let updatedRepoMap = Map.mapWithKey (\filePath (a, b, c, d, tags) ->
            let repoName = getRepoName filePath
                tagString = joinTags (Map.findWithDefault [] repoName updatedTags)
            in (a, b, c, d, tagString)) repoMap
    return updatedRepoMap

getTagsFilePath :: IO FilePath
getTagsFilePath = getConfigPath ("repoconductor" </> "tags.shi")

writeTagsFile :: Map String [String] -> IO ()
writeTagsFile tagMap = do
    tags <- getTagsFilePath
    let updatedLines = map (T.pack . formatLine) $ Map.toList tagMap
    TIO.writeFile tags (T.unlines updatedLines)

parseLine :: String -> (String, [String])
parseLine line =
    let (key, rest) = span (/= '/') line
        values = case rest of
            '/' : vals -> filter (not . null) $ splitOn '/' vals
            _          -> []
    in (key, values)

formatLine :: (String, [String]) -> String
formatLine (key, values) =
    if null values
        then key ++ "/"
        else key ++ "/" ++ intercalate "/" values

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr (\c acc -> if c == delimiter then [] : acc else (c : head acc) : tail acc) [[]]

joinTags :: [String] -> String
joinTags = intercalate "|"

createTags :: FilePath -> IO ()
createTags tags = do
    createDirectoryIfMissing True (takeDirectory tags)
    TIO.writeFile tags ""
