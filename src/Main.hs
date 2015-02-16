module Main where

import System.IO
import REPL

-- | Main loop
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl initState
