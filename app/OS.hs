{-# LANGUAGE CPP #-}

module OS (getSingleChar, silenceOutput, getConfigPath) where

import System.FilePath ((</>))
import System.Environment (getEnv)
import Data.List (isPrefixOf)
import System.Directory (getHomeDirectory)

#ifdef mingw32_HOST_OS
import Foreign.C.Types (CInt(..))
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin)
foreign import ccall unsafe "conio.h getch" c_getch :: IO CInt

getSingleChar :: IO Char
getSingleChar = do
  putStr "\ESC[?25l"
  hSetBuffering stdin NoBuffering
  c <- c_getch 
  putStr "\ESC[?25h"
  return (toEnum $ fromEnum c)
#else
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin)
import System.Posix.Terminal (getTerminalAttributes, setTerminalAttributes, withoutMode, TerminalMode(..))
import System.Posix.IO (stdInput)
import System.Process (callCommand)

getSingleChar :: IO Char
getSingleChar = do
  hSetBuffering stdin NoBuffering
  callCommand "stty raw -echo"
  char <- getChar
  callCommand "stty -raw echo"
  return char
#endif

silenceOutput :: String -> String
#ifdef mingw32_HOST_OS
silenceOutput command
  | "git " `isPrefixOf` command = command ++ " > NUL 2>&1"
  | otherwise = command
#else
silenceOutput command
  | "git " `isPrefixOf` command = command ++ " > /dev/null 2>&1"
  | otherwise = command
#endif

getConfigPath :: String -> IO FilePath
#ifdef mingw32_HOST_OS
getConfigPath path = do
  localAppData <- getEnv "LOCALAPPDATA"
  return $ localAppData </> path 
#else
getConfigPath path = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".config" </> path 
#endif
