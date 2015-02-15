module Main where

import System.IO
import REPL

-- | Main loop
main :: IO ()
main = do hSetBuffering stdout NoBuffering -- ^ turn standard output buffering off so that the prompt flushes before getting input
          repl initState
