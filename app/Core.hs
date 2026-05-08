{-# LANGUAGE OverloadedStrings #-}

module Core (runConductor, ConductorState(..), ConductorStateT) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, when)
import Control.Monad.State (StateT, get, liftIO, put)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist, doesPathExist, getCurrentDirectory, listDirectory, makeAbsolute, pathIsSymbolicLink)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (CreateProcess(..), StdStream(NoStream, CreatePipe), createProcess, proc, readProcess, shell, waitForProcess)
import qualified System.IO as IO
import Text.Printf (printf)
import Text.Read (readMaybe)
import Concurrency (jobLimit, runBounded)
import Taggy (readTags, addTagFiltered, removeTagFiltered, clearTagsFiltered)
import OS (getSingleChar, silenceOutput)
import Purity (filterMap, getPrint, getMaxLength, safeInit, getRepoName)
import Static
import Types (RepoFilters(..), RepoMap, RepoStatus(..), JobState(..))

data ConductorState = ConductorState {
  repoMap      :: RepoMap,
  filters      :: RepoFilters,
  execute      :: String,
  mode         :: String
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
                statusTags = tagString,
                statusJob = JobIdle
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

-- | Run a per-repo command. Captures stderr and returns it on failure.
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
    runGit args = runWithCapturedStderr (proc "git" args) { cwd = Just dir, std_out = NoStream, std_err = CreatePipe }

    runProcessChecked processSpec = do
      (_, _, _, handle) <- createProcess processSpec
      exitCode <- waitForProcess handle
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> ioError $ userError $ "Command failed with exit code " ++ show code

    runWithCapturedStderr processSpec = do
      (_, _, errHandleMaybe, handle) <- createProcess processSpec
      stderrText <- maybe (return "") IO.hGetContents errHandleMaybe
      _ <- length stderrText `seq` return ()
      exitCode <- waitForProcess handle
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure code ->
          ioError $ userError $
            "exit " ++ show code ++ (if null stderrText then "" else ": " ++ trimTrailingNewline stderrText)

trimTrailingNewline :: String -> String
trimTrailingNewline s = reverse (dropWhile (`elem` ("\r\n" :: String)) (reverse s))

isEnter :: Char -> Bool
isEnter char = char == '\r' || char == '\n'

-- ---------------------------------------------------------------------------
-- Live progress runner

-- | Run one async pass over the filtered repos, redrawing the dashboard live.
-- The per-repo IO action should leave job state untouched; the runner will
-- transition Pending -> Running -> Ok/Err and (optionally) update RepoStatus
-- via the provided result handler.
runWithProgress
  :: String                                   -- header label, e.g. "Updating"
  -> RepoMap                                  -- working map (already filtered semantics applied)
  -> [FilePath]                               -- repos to process
  -> (FilePath -> IO (RepoStatus -> RepoStatus))   -- per-repo job, returns status updater
  -> ConductorStateT ()
runWithProgress label workingMap targets perRepoJob = do
  conductorState <- get
  let baselineMap = markPending (repoMap conductorState) targets
  liveVar <- liftIO $ newTVarIO baselineMap
  put conductorState { repoMap = baselineMap }
  limit <- liftIO jobLimit

  rendererHandle <- liftIO $ async (renderLoop label liveVar)

  let onStart key = atomically $ do
        currentMap <- readTVar liveVar
        writeTVar liveVar (Map.adjust (\status -> status { statusJob = JobRunning }) key currentMap)

      onFinish key result = atomically $ do
        currentMap <- readTVar liveVar
        let updater = case result of
              Right statusUpdater -> \status -> (statusUpdater status) { statusJob = JobOk }
              Left  errorMessage  -> \status -> status { statusJob = JobErr errorMessage }
        writeTVar liveVar (Map.adjust updater key currentMap)

      jobs = [(target, perRepoJob target) | target <- targets, Map.member target workingMap]

  results <- liftIO $ runBounded limit jobs onStart onFinish
  liftIO $ cancel rendererHandle

  finalMap <- liftIO $ readTVarIO liveVar
  let clearedMap = Map.map clearJobIfDone finalMap
  updatedState <- get
  put updatedState { repoMap = clearedMap }

  liftIO $ printFailures results

markPending :: RepoMap -> [FilePath] -> RepoMap
markPending repos targets =
  foldr (Map.adjust (\status -> status { statusJob = JobPending })) repos targets

clearJobIfDone :: RepoStatus -> RepoStatus
clearJobIfDone status = case statusJob status of
  JobOk      -> status { statusJob = JobIdle }
  JobPending -> status { statusJob = JobIdle }
  JobRunning -> status { statusJob = JobIdle }
  JobErr _   -> status      -- keep err visible until next pass
  JobIdle    -> status

renderLoop :: String -> TVar RepoMap -> IO ()
renderLoop label liveVar = loop Nothing
  where
    loop previous = do
      snapshot <- readTVarIO liveVar
      when (Just snapshot /= previous) $ paintFrame label snapshot
      threadDelay 100000
      loop (Just snapshot)

paintFrame :: String -> RepoMap -> IO ()
paintFrame label repos = do
  putStr clearScreen
  putStrLn header
  putStrLn ""
  putStrLn (label ++ " ...")
  putStrLn ""
  putStrLn $ printf ("%s%-*s %-15s %-4s %-4s %-4s %-5s %-12s%s")
    (cursive :: String)
    (getMaxLength repos) ("Name" :: String)
    ("Branch" :: String)
    ("<-" :: String)
    ("->" :: String)
    ("M" :: String)
    ("Job" :: String)
    ("Tag" :: String)
    (reset :: String)
  putStrLn ""
  let maxNameLength = getMaxLength repos
  mapM_ putStrLn [getPrint maxNameLength key value | (key, value) <- Map.toList repos]
  hFlush stdout

printFailures :: [(FilePath, Either String a)] -> IO ()
printFailures results = do
  let failures = [(key, message) | (key, Left message) <- results]
  unless (null failures) $ do
    putStrLn ""
    putStrLn (red ++ "Failures:" ++ reset)
    mapM_ printOne failures
  where
    printOne (key, message) =
      putStrLn (" - " ++ red ++ getRepoName key ++ reset ++ ": " ++ message)

-- ---------------------------------------------------------------------------
-- Status refresh

readRepoStatus :: FilePath -> IO (RepoStatus -> RepoStatus)
readRepoStatus dir = do
  statusOutput <- readProcess "git" ["-C", dir, "status", "--porcelain=v2", "--branch"] mempty
  let statusLines = lines statusOutput
      rawHead = headerValue "branch.head" statusLines
      (ahead, behind) = parseAheadBehind (headerValue "branch.ab" statusLines)
      modified = length [line | line <- statusLines, not (null line), not ("#" `isPrefixOf` line)]
  displayHead <- readDisplayHeadName dir rawHead
  return $ \existing -> existing
    { statusBranch = displayHead
    , statusPulls = behind
    , statusPushes = ahead
    , statusModified = modified
    }
  where
    readDisplayHeadName d rawHead
      | rawHead == "(detached)" || rawHead == "HEAD" = readExactTagOrHead d
      | otherwise = return rawHead
    readExactTagOrHead d = do
      tagAttempt <- try (readProcess "git" ["-C", d, "describe", "--tags", "--exact-match", "HEAD"] mempty) :: IO (Either SomeException String)
      return $ either (const "HEAD") tagNameOrHead tagAttempt
    headerValue headerName statusLines =
      case [value | line <- statusLines, Just value <- [stripPrefix ("# " ++ headerName ++ " ") line]] of
        value : _ -> value
        []        -> ""
    parseAheadBehind value = (parseCount "+" value, parseCount "-" value)
    parseCount prefix value =
      case [count | token <- words value, Just raw <- [stripPrefix prefix token], Just count <- [readMaybe raw]] of
        count : _ -> count
        []        -> 0
    firstOutputLine output = case lines output of
      []       -> ""
      line : _ -> line
    tagNameOrHead output = case firstOutputLine output of
      ""      -> "HEAD"
      tagName -> tagName

-- | After an action, the status itself doesn't change here; we just report success.
identityUpdater :: RepoStatus -> RepoStatus
identityUpdater = id

-- ---------------------------------------------------------------------------
-- Main loop

process :: ConductorStateT ()
process = do
  runCommand
  refreshIfNeeded
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

    runCommand :: ConductorStateT ()
    runCommand = do
      conductorState <- get
      let filteredRepos = filterMap (repoMap conductorState) (filters conductorState)
          targets = Map.keys filteredRepos
          cmd = execute conductorState
      case cmd of
          ""       -> return ()
          "update" -> return ()
          c | "tag add " `isPrefixOf` c -> do
                let tagName = drop (length ("tag add " :: String)) c
                updatedRepoMap <- liftIO $ addTagFiltered tagName filteredRepos (repoMap conductorState)
                put conductorState { repoMap = updatedRepoMap }
            | "tag remove " `isPrefixOf` c -> do
                let tagName = drop (length ("tag remove " :: String)) c
                updatedRepoMap <- liftIO $ removeTagFiltered tagName filteredRepos (repoMap conductorState)
                put conductorState { repoMap = updatedRepoMap }
            | "tag clear" == c -> do
                updatedRepoMap <- liftIO $ clearTagsFiltered filteredRepos (repoMap conductorState)
                put conductorState { repoMap = updatedRepoMap }
          "lazygit" -> liftIO $ mapM_ (runLazygitOne (execute conductorState)) targets
          _         -> runWithProgress (actionLabel cmd) filteredRepos targets $ \dir -> do
                          runRepoCommand cmd dir
                          return identityUpdater

    refreshIfNeeded :: ConductorStateT ()
    refreshIfNeeded = do
      conductorState <- get
      let cmd = execute conductorState
          isStatusRelevant = "git" `isInfixOf` cmd || cmd == "update"
      when isStatusRelevant $ do
        let filteredRepos = filterMap (repoMap conductorState) (filters conductorState)
            targets = Map.keys filteredRepos
        runWithProgress "Refreshing" filteredRepos targets readRepoStatus

    runLazygitOne :: String -> FilePath -> IO ()
    runLazygitOne cmd dir = do
      result <- try (runRepoCommand cmd dir) :: IO (Either SomeException ())
      case result of
        Right _ -> return ()
        Left  _ -> return ()

    actionLabel :: String -> String
    actionLabel cmd
      | cmd == "git fetch"                             = "Fetching"
      | cmd == "git pull"                              = "Pulling"
      | "git switch " `isPrefixOf` cmd                 = "Switching"
      | "git commit-and-push " `isPrefixOf` cmd        = "Committing and pushing"
      | otherwise                                      = "Running"

    displayColumnNames :: ConductorStateT ()
    displayColumnNames = do
      conductorState <- get
      liftIO $ do
        putStr "\r \r"
        hFlush stdout
        putStrLn $ printf ("%s%-*s %-15s %-4s %-4s %-4s %-5s %-12s%s")
          (cursive :: String)
          (getMaxLength (repoMap conductorState)) ("Name" :: String)
          ("Branch" :: String)
          ("<-" :: String)
          ("->" :: String)
          ("M" :: String)
          ("Job" :: String)
          ("Tag" :: String)
          (reset :: String)
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
