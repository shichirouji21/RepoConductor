{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State (evalStateT)
import Core (runConductor, ConductorState(..))
import Data.Version (showVersion)
import Options.Applicative
import Paths_repoconductor (version)
import Types (RepoFilters(..), ConductorOptions(..), defaultOptions)

data ConductorCommand = RunConductor ConductorOptions

versionString :: String
versionString = "repoconductor " ++ showVersion version

versionOption :: Parser (a -> a)
versionOption =
  infoOption versionString
    ( long "version"
   <> short 'V'
   <> help "Show version and exit" )

optionsParser :: Parser ConductorOptions
optionsParser = ConductorOptions
  <$> optional (strOption
        ( long "path"
       <> short 'p'
       <> metavar "DIR"
       <> help "Scan DIR instead of the current working directory" ))
  <*> flag True False
        ( long "no-recurse"
       <> help "Do not recurse: only treat immediate children of the path as candidates" )
  <*> optional (option auto
        ( long "jobs"
       <> short 'j'
       <> metavar "N"
       <> help "Maximum concurrent git jobs (default: max(4, capabilities * 2))" ))
  <*> flag True False
        ( long "no-startup-refresh"
       <> help "Skip the initial status refresh; rows stay empty until U is pressed" )
  <*> switch
        ( long "debug"
       <> help "Verbose failure output (full captured stderr per failure)" )

conductorCommandParser :: Parser ConductorCommand
conductorCommandParser = RunConductor <$> optionsParser

main :: IO ()
main = do
  parsedCommand <- execParser opts
  case parsedCommand of
    RunConductor userOptions -> evalStateT runConductor (initialState userOptions)
  where
    opts = info (helper <*> versionOption <*> conductorCommandParser)
      ( fullDesc
     <> progDesc "Manage multiple Git repositories from a terminal dashboard"
     <> header   ("RepoConductor " ++ showVersion version ++ " - multi-repo dashboard") )
    initialState userOptions = ConductorState {
      repoMap      = mempty,
      filters      = RepoFilters {
        filterDirty = False,
        filterName = mempty,
        filterBranch = mempty,
        filterTag = mempty
      },
      execute      = "update",
      mode         = "normal",
      options      = mergeOptions userOptions
    }
    -- Allow --no-recurse / --no-startup-refresh to coexist with the flag-based
    -- defaults from defaultOptions for any future fields not yet covered.
    mergeOptions userOptions = defaultOptions {
        optionPath           = optionPath userOptions
      , optionRecurse        = optionRecurse userOptions
      , optionJobs           = optionJobs userOptions
      , optionStartupRefresh = optionStartupRefresh userOptions
      , optionDebug          = optionDebug userOptions
      }
