{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Tommy (readConfig) where

import Toml (TomlCodec, (.=), decodeFileEither, table, arrayOf, _String, TomlDecodeError)
import Data.Text (Text, pack)
import Toml.Type.Key (Key)
import Control.Monad (filterM)
import System.Directory (doesPathExist)
import OS (getConfigPath)
import qualified Data.Text.IO as T
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Control.Monad (unless)
import qualified Data.Text.IO as T
import Data.Text (pack)

defaultConfig :: String
defaultConfig = "[main]\npaths = [\"C:\\\\repositories\\\\\", \"/home/satsuko/repositories\"]\n"

data Paths = Paths { paths :: [FilePath] }

pathCodec :: TomlCodec Paths
pathCodec = Toml.table codec "main"
  where
    codec = Paths <$> Toml.arrayOf Toml._String "paths" .= paths

readConfig :: IO [FilePath]
readConfig = do
  config <- getConfigPath ("repoconductor" </> "config.toml")
  configExists <- doesFileExist config 
  unless configExists $ createConfig config
  result <- decodeFileEither pathCodec config
  case result of
    Left error -> do
      putStrLn $ "Error reading config: " ++ show error
      return []
    Right pathsData -> do
      existingFilePaths <- filterM doesPathExist (paths pathsData)
      return existingFilePaths

createConfig :: FilePath -> IO ()
createConfig config = do
  createDirectoryIfMissing True (takeDirectory config)
  T.writeFile config (pack defaultConfig)
