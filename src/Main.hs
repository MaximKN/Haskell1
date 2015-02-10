module Main where

import System.IO
import Parsing
import Expr
import REPL

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl initState
