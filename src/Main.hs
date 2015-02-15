module Main where

import System.IO
import REPL

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl initState
