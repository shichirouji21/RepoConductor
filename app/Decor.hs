{-# LANGUAGE CPP #-}

module Decor (spinner) where

import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

spinner :: IO ()
spinner = loop ["[RepoConductor] -", "[RepoConductor] \\", "[RepoConductor] |", "[RepoConductor] /"]
  where
    loop :: [String] -> IO ()
    loop frames = do
      case frames of
        [] -> loop ["[RepoConductor] -", "[RepoConductor] \\", "[RepoConductor] |", "[RepoConductor] /"]
        (frame:rest) -> do
          putStr ("\r" ++ frame)
          hFlush stdout
          threadDelay 100000
          loop rest
