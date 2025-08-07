{-# LANGUAGE OverloadedStrings #-}

module Core (runConductor, ConductorState(..), ConductorStateT) where

import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_)
import Control.Exception (try, SomeException)
import Control.Monad.State
import Data.List (isInfixOf, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (callCommand, readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Options.Applicative
import Tommy (readConfig)
import Taggy (readTags, addTagFiltered, removeTagFiltered, clearTagsFiltered)
import OS (getSingleChar, getConfigPath, silenceOutput)
import Purity (filterMap, getPrint, getMaxLength, safeInit, getRepoName, bold, replaceFirst)
import Decor (spinner)
import Static

data ConductorState = ConductorState {
  repoMap      :: Map FilePath (String, Int, Int, Int, String),
  filters      :: (Bool, String, String, String),
  execute      :: String,
  mode         :: String,
  spinnerAsync :: Maybe (Async ())
}

type ConductorStateT = StateT ConductorState IO

runConductor :: ConductorStateT ()
runConductor = do
  paths <- liftIO readConfig
  fire paths

fire :: [FilePath] -> ConductorStateT ()
fire [] = do
  configPath <- liftIO $ getConfigPath ("repoconductor" </> "config.toml")
  liftIO $ putStrLn "No Git repositories found. Add relevant paths to the config file:"
  liftIO $ putStrLn configPath
fire paths = do
  initialize paths
  process

initialize :: [FilePath] -> ConductorStateT ()
initialize paths = do
  state <- get
  folders <- liftIO $ mapM (\path -> do
    files <- listDirectory path
    mapM (makeAbsolute . (path </>)) files) paths
  gitFolders <- liftIO $ filterM (\dir -> doesPathExist (dir </> ".git")) (concat folders)
  tags <- liftIO $ readTags (map getRepoName gitFolders)
  let updatedRepoMap = Map.fromList
            [ (folder, ("", 0, 0, 0, tagString))
            | folder <- gitFolders
            , let repoName = getRepoName folder
                  tagList = Map.findWithDefault [] repoName tags
                  tagString = if null tagList then "" else concatMap (++ "|") (init tagList) ++ last tagList
            ]
  put state { repoMap = updatedRepoMap }

process :: ConductorStateT ()
process = do
  startSpinner
  runCommand
  updateMap
  stopSpinner
  displayHeader
  displayColumnNames
  displayResults
  clearCommand
  state <- get
  getFeedback (mode state)
  where
    displayHeader :: ConductorStateT ()
    displayHeader = liftIO $ do
      putStr clearScreen
      hFlush stdout
      putStrLn Static.header 
      putStrLn mempty
    startSpinner :: ConductorStateT ()
    startSpinner = do
      liftIO $ putStr clearScreen
      state <- get
      case spinnerAsync state of
        Just _ -> do
          return ()
        Nothing -> do
          asyncHandle <- liftIO $ async spinner
          put state { spinnerAsync = Just asyncHandle }
    runCommand :: ConductorStateT ()
    runCommand = do
      state <- get
      let filteredRepos = filterMap (repoMap state) (filters state)
      case (execute state) of
          ""       -> return ()
          "update" -> return ()
          cmd
              | "tag add " `isPrefixOf` cmd -> do
                  let tagName = drop (length ("tag add " :: String)) cmd
                  updatedRepoMap <- liftIO $ addTagFiltered tagName filteredRepos (repoMap state)
                  put state { repoMap = updatedRepoMap }
              | "tag remove " `isPrefixOf` cmd -> do
                  let tagName = drop (length ("tag remove " :: String)) cmd
                  updatedRepoMap <- liftIO $ removeTagFiltered tagName filteredRepos (repoMap state)
                  put state { repoMap = updatedRepoMap }
              | "tag clear" == cmd -> do
                  updatedRepoMap <- liftIO $ clearTagsFiltered filteredRepos (repoMap state)
                  put state { repoMap = updatedRepoMap }
          "lazygit" -> do
              let runRepoCommand (_, dir) = do
                      result <- try (callCommand $ "cd " ++ dir ++ " && " ++ (silenceOutput (execute state))) :: IO (Either SomeException ())
                      case result of
                          Right _ -> return ()
                          Left  _ -> return ()
              liftIO $ mapM_ runRepoCommand (zip [0..] (Map.keys filteredRepos))
          _ -> do
              let runRepoCommand (_, dir) = do
                      result <- try (callCommand $ "cd " ++ dir ++ " && " ++ (silenceOutput (execute state))) :: IO (Either SomeException ())
                      case result of
                          Right _ -> return ()
                          Left  _ -> return ()
              liftIO $ mapConcurrently_ runRepoCommand (zip [0..] (Map.keys filteredRepos))
    updateMap :: ConductorStateT () 
    updateMap = do
      state <- get
      case () of
        _ | not ("git" `isInfixOf` (execute state)) && (execute state) /= "update" -> return ()
          | otherwise -> do
              updatedRepoMap <- Map.traverseWithKey updateUnit (repoMap state)
              put state { repoMap = updatedRepoMap }
      where
        updateUnit :: FilePath -> (String, Int, Int, Int, String) -> StateT ConductorState IO (String, Int, Int, Int, String)
        updateUnit dir (branch, count1, count2, count3, tag) = do
          state <- get
          case (not (Map.member dir (filterMap (repoMap state) (filters state)))) of
            True  -> return (branch, count1, count2, count3, tag)
            False -> do
              branch       <- liftIO $ try (readProcess "git" ["-C", dir, "rev-parse", "--abbrev-ref", "HEAD"]        mempty) :: StateT ConductorState IO (Either SomeException String)
              pullResult   <- liftIO $ try (readProcess "git" ["-C", dir, "rev-list", "--count", "HEAD..@{upstream}"] mempty) :: StateT ConductorState IO (Either SomeException String) 
              pushResult   <- liftIO $ try (readProcess "git" ["-C", dir, "rev-list", "--count", "@{upstream}..HEAD"] mempty) :: StateT ConductorState IO (Either SomeException String) 
              statusResult <- liftIO $ try (readProcess "git" ["-C", dir, "status", "--porcelain"]                    mempty) :: StateT ConductorState IO (Either SomeException String)
              return (
                either (const "") init branch,
                either (const  0) (fromMaybe 0 . readMaybe . init) pullResult,
                either (const  0) (fromMaybe 0 . readMaybe . init) pushResult,
                either (const  0) (length . lines) statusResult,
                tag)
    stopSpinner :: ConductorStateT ()
    stopSpinner = do
      state <- get
      case spinnerAsync state of
        Just asyncHandle -> do
          liftIO $ cancel asyncHandle
          put state { spinnerAsync = Nothing }
        Nothing -> return ()
    displayColumnNames :: ConductorStateT ()
    displayColumnNames = do
      state <- get
      liftIO $ do
        putStr "\r \r"
        hFlush stdout
        putStrLn $ printf ("%s%-*s %-15s %-4s %-4s %-4s %-12s%s") (cursive :: String) (getMaxLength (repoMap state)) ("Name" :: String) ("Branch" :: String) ("<-" :: String) ("->" :: String) ("M" :: String) ("Tag" :: String) (reset :: String)
        putStrLn mempty
    displayResults :: ConductorStateT ()
    displayResults = do
      state <- get
      liftIO $ mapM_ putStrLn (catMaybes (map (\(key, value) -> getPrint (getMaxLength (repoMap state)) key value) $ Map.toList (filterMap (repoMap state) (filters state)))) 
    clearCommand :: ConductorStateT ()
    clearCommand = do
      state <- get
      put state { execute = mempty } 
    getFeedback :: String -> ConductorStateT ()
    getFeedback "fName" = do
      state <- get
      let (fDirty, fName, fBranch, fTag) = filters state
      liftIO $ putStrLn ("\nName: " ++ fName)
      userAction <- liftIO getSingleChar
      case userAction of
        '\r' -> do
          put state { mode = "normal" }
          process
        '\DEL' -> do
          put state { filters = (fDirty, safeInit fName, fBranch, fTag) } 
          process
        '\BS'  -> do
          put state { filters = (fDirty, safeInit fName, fBranch, fTag) } 
          process
        _ -> do
          put state { filters = (fDirty, fName ++ [userAction], fBranch, fTag) }
          process
    getFeedback "fBranch" = do
      state <- get
      let (fDirty, fName, fBranch, fTag) = filters state
      liftIO $ putStrLn ("\nBranch: " ++ fBranch)
      userAction <- liftIO getSingleChar
      case userAction of
        '\r' -> do
          put state { mode = "normal" }
          process
        '\DEL' -> do
          put state { filters = (fDirty, fName, safeInit fBranch, fTag) } 
          process
        '\BS'  -> do
          put state { filters = (fDirty, fName, safeInit fBranch, fTag) } 
          process
        _ -> do
          put state { filters = (fDirty, fName, fBranch ++ [userAction], fTag) }
          process
    getFeedback "fTag" = do
      state <- get
      let (fDirty, fName, fBranch, fTag) = filters state
      liftIO $ putStrLn ("\nTag: " ++ fTag)
      userAction <- liftIO getSingleChar
      case userAction of
        '\r' -> do
          put state { mode = "normal" }
          process
        '\DEL' -> do
          put state { filters = (fDirty, fName, fBranch, safeInit fTag) } 
          process
        '\BS'  -> do
          put state { filters = (fDirty, fName, fBranch, safeInit fTag) } 
          process
        _ -> do
          put state { filters = (fDirty, fName, fBranch, fTag ++ [userAction]) }
          process
    getFeedback mode = do
      liftIO $ putStrLn mainOptions 
      userAction <- liftIO getSingleChar
      state <- get
      let (fDirty, fName, fBranch, fTag) = filters state
      case userAction of
        'f' -> do
          --liftIO $ putStrLn filterOptions 
          liftIO $
            if fDirty
              then printWithBold filterOptions "(D)irty"   -- highlight Dirty
              else putStrLn        filterOptions           -- plain output
          userActionFilter <- liftIO getSingleChar
          newFilters <- case userActionFilter of
                          'a' -> return (False, mempty, mempty, mempty)
                          'd' -> return (not fDirty, fName, fBranch, fTag)
                          'n' -> do
                            put state { mode = "fName" }
                            return (fDirty, fName, fBranch, fTag)
                          'b' -> do
                            put state { mode = "fBranch" }
                            return (fDirty, fName, fBranch, fTag)
                          't' -> do
                            put state { mode = "fTag" }
                            return (fDirty, fName, fBranch, fTag)
                          _   -> return (fDirty, fName, fBranch, fTag)
          state <- get
          put state { filters = newFilters }
          process
        'a' -> do
          liftIO $ putStrLn actionOptions 
          userActionCommand <- liftIO getSingleChar
          newCommand <- case userActionCommand of
                      'f' -> return "git fetch"
                      'p' -> return "git pull"
                      's' -> do
                        liftIO $ putStrLn "Branch: "
                        branch <- liftIO getLine
                        return ("git switch " ++ branch)
                      'c' -> do
                        liftIO $ putStrLn "Message: "
                        message <- liftIO getLine
                        return ("git add . && git commit -m \"" ++ message ++ "\" && git push")
                      'l' -> return "lazygit"
                      'm' -> do
                        liftIO $ putStrLn "Command:"
                        liftIO getLine
                      _   -> return mempty
          put state { execute = newCommand }
          process
        'u' -> do
          put state { execute = "update" }
          process
        't' -> do
          liftIO $ putStrLn "(A)dd, (R)emove, (C)lear"
          userTagCommand <- liftIO getSingleChar
          tagCommand <- case userTagCommand of
                    'a' -> do
                      liftIO $ putStrLn "Tag:"
                      tagName <- liftIO getLine
                      return ("tag add " ++ tagName)
                    'r' -> do
                      liftIO $ putStrLn "Tag:"
                      tagName <- liftIO getLine
                      return ("tag remove " ++ tagName)
                    'c' -> return ("tag clear")
                    _ -> return ""
          put state { execute = tagCommand }
          process
        'q' -> do
          liftIO $ putStrLn goodbye 
        _   -> process

printWithBold :: String      -- ^ full text
              -> String      -- ^ fragment to make bold
              -> IO ()
printWithBold fullText toBold =
  putStrLn $ replaceFirst toBold (bold toBold) fullText
