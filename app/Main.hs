{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State (evalStateT)
import Core (runConductor, ConductorState(..))
import Options.Applicative (Parser, execParser, fullDesc, header, helper, info, progDesc)
import Types (RepoFilters(..))

data ConductorCommand = RunConductor

conductorCommandParser :: Parser ConductorCommand
conductorCommandParser = pure RunConductor

main :: IO ()
main = do
  command <- execParser opts
  case command of
    RunConductor -> evalStateT runConductor initialState
  where
    opts = info (helper <*> conductorCommandParser)
      ( fullDesc
     <> progDesc "Manage multiple repositories"
     <> header   "RepoConductor - A tool to manage multiple git repositories" )
    initialState = ConductorState {
      repoMap      = mempty,
      filters      = RepoFilters {
        filterDirty = False,
        filterName = mempty,
        filterBranch = mempty,
        filterTag = mempty
      },
      execute      = "update",
      mode         = "normal"
    }
