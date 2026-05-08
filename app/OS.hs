{-# LANGUAGE CPP #-}

module OS (getSingleChar, silenceOutput, getConfigPath) where

import Control.Exception (bracket)
import Data.List (isPrefixOf)
import System.FilePath ((</>))
import System.IO (BufferMode(NoBuffering), hGetBuffering, hSetBuffering, stdin)

#ifdef mingw32_HOST_OS
import Control.Exception (bracket_)
import Foreign.C.Types (CInt(..))
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
foreign import ccall unsafe "conio.h getch" c_getch :: IO CInt

getSingleChar :: IO Char
getSingleChar = withNoBuffering $
  bracket_ hideCursor showCursor $
    toEnum . fromEnum <$> c_getch
  where
    hideCursor = putStr "\ESC[?25l" >> hFlush stdout
    showCursor = putStr "\ESC[?25h" >> hFlush stdout
#else
import System.Directory (getHomeDirectory)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalMode(EnableEcho, ProcessInput), TerminalState(Immediately), getTerminalAttributes, setTerminalAttributes, withoutMode)

getSingleChar :: IO Char
getSingleChar = withNoBuffering $
  bracket
    (getTerminalAttributes stdInput)
    (\attrs -> setTerminalAttributes stdInput attrs Immediately)
    (\attrs -> do
      setTerminalAttributes stdInput (withoutMode (withoutMode attrs ProcessInput) EnableEcho) Immediately
      getChar)
#endif

withNoBuffering :: IO a -> IO a
withNoBuffering action =
  bracket
    (hGetBuffering stdin)
    (hSetBuffering stdin)
    (\_ -> hSetBuffering stdin NoBuffering >> action)

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
