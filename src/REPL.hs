module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [] 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars n v xs = (n,v):dropVar n xs

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar _ [] = []
dropVar n ((x,_):xs) | n == x = xs
                     | otherwise = dropVar n xs

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- Get the most recent command from the history
getCommand :: State -> Int -> Command
getCommand st n = history st !! (n - 1)

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' = undefined
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Eval e) 
     = do let st' = undefined
          -- Print the result of evaluation
          repl st'

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                         process st cmd
                  _ -> if inp == ":q"  then putStrLn "Bye" else -- Quit command
                       if '!' `elem` inp then putStrLn (show (getCommand st n)) 
                       else do putStrLn "Parse error"
                               repl st
                       where n = read (drop 1 inp) :: Int
