{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Core (runConductor, ConductorState(..))
import Options.Applicative

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
      filters      = (False, mempty, mempty, mempty),
      execute      = "git fetch",
      mode         = "normal",
      spinnerAsync = Nothing
    }
