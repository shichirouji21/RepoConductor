{-# LANGUAGE OverloadedStrings #-}

module Core (runConductor, ConductorState(..), ConductorStateT) where

import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_)
import Control.Exception (try, SomeException)
import Control.Monad (filterM)
import Control.Monad.State (StateT, get, liftIO, put)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist, doesPathExist, getCurrentDirectory, listDirectory, makeAbsolute, pathIsSymbolicLink)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (CreateProcess(..), StdStream(NoStream), createProcess, proc, readProcess, shell, waitForProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Taggy (readTags, addTagFiltered, removeTagFiltered, clearTagsFiltered)
import OS (getSingleChar, silenceOutput)
import Purity (filterMap, getPrint, getMaxLength, safeInit)
import Decor (spinner)
import Static
import Types (RepoFilters(..), RepoMap, RepoStatus(..))

data ConductorState = ConductorState {
  repoMap      :: RepoMap,
  filters      :: RepoFilters,
  execute      :: String,
  mode         :: String,
  spinnerAsync :: Maybe (Async ())
}

type ConductorStateT = StateT ConductorState IO

data RepoCommand
  = GitFetch
  | GitPull
  | GitSwitch String
  | GitCommitPush String
  | Lazygit
  | Manual String

runConductor :: ConductorStateT ()
runConductor = do
  root <- liftIO getCurrentDirectory
  fire root

fire :: FilePath -> ConductorStateT ()
fire root = do
  gitFolders <- liftIO $ discoverGitRepos root
  case gitFolders of
    [] -> liftIO $ putStrLn $ "No Git repositories found under " ++ root
    _  -> do
      initialize gitFolders
      process

initialize :: [FilePath] -> ConductorStateT ()
initialize gitFolders = do
  conductorState <- get
  tags <- liftIO $ readTags gitFolders
  let updatedRepoMap = Map.fromList
            [ (folder, RepoStatus {
                statusBranch = "",
                statusPulls = 0,
                statusPushes = 0,
                statusModified = 0,
                statusTags = tagString
              })
            | folder <- gitFolders
            , let tagList = Map.findWithDefault [] folder tags
                  tagString = if null tagList then "" else concatMap (++ "|") (init tagList) ++ last tagList
            ]
  put conductorState { repoMap = updatedRepoMap }

discoverGitRepos :: FilePath -> IO [FilePath]
discoverGitRepos root = makeAbsolute root >>= go
  where
    go dir = do
      isRepo <- doesPathExist (dir </> ".git")
      if isRepo
        then return [dir]
        else do
          childDirs <- listChildDirectories dir
          concat <$> mapM go childDirs

    listChildDirectories dir = do
      result <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case result of
        Left _ -> return []
        Right names -> filterM shouldSearch [dir </> name | name <- names, name /= ".git"]

    shouldSearch path = do
      isDir <- doesDirectoryExist path
      if not isDir
        then return False
        else do
          isLink <- pathIsSymbolicLink path
          return (not isLink)

parseRepoCommand :: String -> RepoCommand
parseRepoCommand command
  | command == "git fetch" = GitFetch
  | command == "git pull" = GitPull
  | "git switch " `isPrefixOf` command = GitSwitch (drop (length ("git switch " :: String)) command)
  | "git commit-and-push " `isPrefixOf` command = GitCommitPush (drop (length ("git commit-and-push " :: String)) command)
  | command == "lazygit" = Lazygit
  | otherwise = Manual command

runRepoCommand :: String -> FilePath -> IO ()
runRepoCommand command dir =
  case parseRepoCommand command of
    GitFetch -> runGit ["fetch"]
    GitPull -> runGit ["pull"]
    GitSwitch branch -> runGit ["switch", branch]
    GitCommitPush message -> do
      runGit ["add", "."]
      runGit ["commit", "-m", message]
      runGit ["push"]
    Lazygit -> runProcessChecked $ (proc "lazygit" []) { cwd = Just dir }
    Manual manualCommand -> runProcessChecked $ (shell (silenceOutput manualCommand)) { cwd = Just dir }
  where
    runGit args = runProcessChecked $ (proc "git" args) { cwd = Just dir, std_out = NoStream, std_err = NoStream }

    runProcessChecked processSpec = do
      (_, _, _, handle) <- createProcess processSpec
      exitCode <- waitForProcess handle
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> ioError $ userError $ "Command failed with exit code " ++ show code

isEnter :: Char -> Bool
isEnter char = char == '\r' || char == '\n'

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
  conductorState <- get
  getFeedback (mode conductorState)
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
      conductorState <- get
      case spinnerAsync conductorState of
        Just _ -> do
          return ()
        Nothing -> do
          asyncHandle <- liftIO $ async spinner
          put conductorState { spinnerAsync = Just asyncHandle }
    runCommand :: ConductorStateT ()
    runCommand = do
      conductorState <- get
      let filteredRepos = filterMap (repoMap conductorState) (filters conductorState)
      case (execute conductorState) of
          ""       -> return ()
          "update" -> return ()
          cmd
              | "tag add " `isPrefixOf` cmd -> do
                  let tagName = drop (length ("tag add " :: String)) cmd
                  updatedRepoMap <- liftIO $ addTagFiltered tagName filteredRepos (repoMap conductorState)
                  put conductorState { repoMap = updatedRepoMap }
              | "tag remove " `isPrefixOf` cmd -> do
                  let tagName = drop (length ("tag remove " :: String)) cmd
                  updatedRepoMap <- liftIO $ removeTagFiltered tagName filteredRepos (repoMap conductorState)
                  put conductorState { repoMap = updatedRepoMap }
              | "tag clear" == cmd -> do
                  updatedRepoMap <- liftIO $ clearTagsFiltered filteredRepos (repoMap conductorState)
                  put conductorState { repoMap = updatedRepoMap }
          "lazygit" -> do
              let runLazygit dir = do
                      result <- try (runRepoCommand (execute conductorState) dir) :: IO (Either SomeException ())
                      case result of
                          Right _ -> return ()
                          Left  _ -> return ()
              liftIO $ mapM_ runLazygit (Map.keys filteredRepos)
          _ -> do
              let runCommandForRepo dir = do
                      result <- try (runRepoCommand (execute conductorState) dir) :: IO (Either SomeException ())
                      case result of
                          Right _ -> return ()
                          Left  _ -> return ()
              liftIO $ mapConcurrently_ runCommandForRepo (Map.keys filteredRepos)
    updateMap :: ConductorStateT () 
    updateMap = do
      conductorState <- get
      case () of
        _ | not ("git" `isInfixOf` (execute conductorState)) && (execute conductorState) /= "update" -> return ()
          | otherwise -> do
              updatedRepoMap <- Map.traverseWithKey updateUnit (repoMap conductorState)
              put conductorState { repoMap = updatedRepoMap }
      where
        updateUnit :: FilePath -> RepoStatus -> StateT ConductorState IO RepoStatus
        updateUnit dir currentStatus = do
          conductorState <- get
          case (not (Map.member dir (filterMap (repoMap conductorState) (filters conductorState)))) of
            True  -> return currentStatus
            False -> do
              statusResult <- liftIO $ try (readRepoStatus dir (statusTags currentStatus)) :: StateT ConductorState IO (Either SomeException RepoStatus)
              return $ either (const currentStatus) id statusResult
        readRepoStatus :: FilePath -> String -> IO RepoStatus
        readRepoStatus dir repoTags = do
          statusOutput <- readProcess "git" ["-C", dir, "status", "--porcelain=v2", "--branch"] mempty
          let statusLines = lines statusOutput
              rawHead = headerValue "branch.head" statusLines
              (ahead, behind) = parseAheadBehind (headerValue "branch.ab" statusLines)
              modified = length [line | line <- statusLines, not (null line), not ("#" `isPrefixOf` line)]
          displayHead <- readDisplayHeadName dir rawHead
          return $ RepoStatus displayHead behind ahead modified repoTags
        readDisplayHeadName :: FilePath -> String -> IO String
        readDisplayHeadName dir rawHead
          | rawHead == "(detached)" || rawHead == "HEAD" = readExactTagOrHead dir
          | otherwise = return rawHead
        readExactTagOrHead :: FilePath -> IO String
        readExactTagOrHead dir = do
          tagName <- try (readProcess "git" ["-C", dir, "describe", "--tags", "--exact-match", "HEAD"] mempty) :: IO (Either SomeException String)
          return $ either (const "HEAD") tagNameOrHead tagName
        headerValue :: String -> [String] -> String
        headerValue headerName statusLines =
          case [value | line <- statusLines, Just value <- [stripPrefix ("# " ++ headerName ++ " ") line]] of
            value : _ -> value
            []        -> ""
        parseAheadBehind :: String -> (Int, Int)
        parseAheadBehind value = (parseCount "+" value, parseCount "-" value)
        parseCount :: String -> String -> Int
        parseCount prefix value =
          case [count | token <- words value, Just raw <- [stripPrefix prefix token], Just count <- [readMaybe raw]] of
            count : _ -> count
            []        -> 0
        firstOutputLine :: String -> String
        firstOutputLine output = case lines output of
          []       -> ""
          line : _ -> line
        tagNameOrHead :: String -> String
        tagNameOrHead output = case firstOutputLine output of
          ""      -> "HEAD"
          tagName -> tagName
    stopSpinner :: ConductorStateT ()
    stopSpinner = do
      conductorState <- get
      case spinnerAsync conductorState of
        Just asyncHandle -> do
          liftIO $ cancel asyncHandle
          put conductorState { spinnerAsync = Nothing }
        Nothing -> return ()
    displayColumnNames :: ConductorStateT ()
    displayColumnNames = do
      conductorState <- get
      liftIO $ do
        putStr "\r \r"
        hFlush stdout
        putStrLn $ printf ("%s%-*s %-15s %-4s %-4s %-4s %-12s%s") (cursive :: String) (getMaxLength (repoMap conductorState)) ("Name" :: String) ("Branch" :: String) ("<-" :: String) ("->" :: String) ("M" :: String) ("Tag" :: String) (reset :: String)
        putStrLn mempty
    displayResults :: ConductorStateT ()
    displayResults = do
      conductorState <- get
      let maxNameLength = getMaxLength (repoMap conductorState)
          visibleRepos = filterMap (repoMap conductorState) (filters conductorState)
      liftIO $ mapM_ putStrLn [getPrint maxNameLength key value | (key, value) <- Map.toList visibleRepos]
    clearCommand :: ConductorStateT ()
    clearCommand = do
      conductorState <- get
      put conductorState { execute = mempty } 
    getFeedback :: String -> ConductorStateT ()
    getFeedback "fName" = do
      conductorState <- get
      let currentFilters = filters conductorState
      liftIO $ putStrLn ("\nName: " ++ filterName currentFilters)
      userAction <- liftIO getSingleChar
      case userAction of
        char | isEnter char -> do
          put conductorState { mode = "normal" }
          process
        '\DEL' -> do
          put conductorState { filters = currentFilters { filterName = safeInit (filterName currentFilters) } } 
          process
        '\BS'  -> do
          put conductorState { filters = currentFilters { filterName = safeInit (filterName currentFilters) } } 
          process
        _ -> do
          put conductorState { filters = currentFilters { filterName = filterName currentFilters ++ [userAction] } }
          process
    getFeedback "fBranch" = do
      conductorState <- get
      let currentFilters = filters conductorState
      liftIO $ putStrLn ("\nBranch: " ++ filterBranch currentFilters)
      userAction <- liftIO getSingleChar
      case userAction of
        char | isEnter char -> do
          put conductorState { mode = "normal" }
          process
        '\DEL' -> do
          put conductorState { filters = currentFilters { filterBranch = safeInit (filterBranch currentFilters) } } 
          process
        '\BS'  -> do
          put conductorState { filters = currentFilters { filterBranch = safeInit (filterBranch currentFilters) } } 
          process
        _ -> do
          put conductorState { filters = currentFilters { filterBranch = filterBranch currentFilters ++ [userAction] } }
          process
    getFeedback "fTag" = do
      conductorState <- get
      let currentFilters = filters conductorState
      liftIO $ putStrLn ("\nTag: " ++ filterTag currentFilters)
      userAction <- liftIO getSingleChar
      case userAction of
        char | isEnter char -> do
          put conductorState { mode = "normal" }
          process
        '\DEL' -> do
          put conductorState { filters = currentFilters { filterTag = safeInit (filterTag currentFilters) } } 
          process
        '\BS'  -> do
          put conductorState { filters = currentFilters { filterTag = safeInit (filterTag currentFilters) } } 
          process
        _ -> do
          put conductorState { filters = currentFilters { filterTag = filterTag currentFilters ++ [userAction] } }
          process
    getFeedback _ = do
      liftIO $ putStrLn mainOptions 
      userAction <- liftIO getSingleChar
      conductorState <- get
      let currentFilters = filters conductorState
      case userAction of
        'f' -> do
          liftIO $ putStrLn filterOptions 
          userActionFilter <- liftIO getSingleChar
          newFilters <- case userActionFilter of
                          'a' -> return (RepoFilters False mempty mempty mempty)
                          'd' -> return currentFilters { filterDirty = not (filterDirty currentFilters) }
                          'n' -> do
                            put conductorState { mode = "fName" }
                            return currentFilters
                          'b' -> do
                            put conductorState { mode = "fBranch" }
                            return currentFilters
                          't' -> do
                            put conductorState { mode = "fTag" }
                            return currentFilters
                          _   -> return currentFilters
          updatedState <- get
          put updatedState { filters = newFilters }
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
                        return ("git commit-and-push " ++ message)
                      'l' -> return "lazygit"
                      'm' -> do
                        liftIO $ putStrLn "Command:"
                        liftIO getLine
                      _   -> return mempty
          put conductorState { execute = newCommand }
          process
        'u' -> do
          put conductorState { execute = "update" }
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
          put conductorState { execute = tagCommand }
          process
        'q' -> do
          liftIO $ putStrLn goodbye 
        _   -> process
