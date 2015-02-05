module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
it :: Int -- Implicitly stores the result of the last operation

initState = State [] 0 []
it = 0

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
getCommand st n = history st !! n

-- Get value from the state by name
getValue :: State -> Name -> Int
getValue st n = getValue1 n $ vars st

-- Sub-function for a getValue that gets a value by name in a list
getValue1 :: Name -> [(Name, Int)] -> Int
getValue1 _ [] = 0
getValue1 n ((x,y):xs) | n == x = y
                       | otherwise = getValue1 n xs

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let val = getValue st var
              st' x = updateState (addHistory st (Set var e)) var x
              in case eval [(var, val)] e of
                  Just x -> do putStrLn "OK"
                               repl $ st' x
                  Nothing -> putStrLn "val - previous v / Add result of evaluating e to the history"
          -- st' should include the variable set to the result of evaluating e
process st (Eval e) 
     = do let st' = undefined
          -- Print the result of evaluation
          repl st'

updateState :: State -> Name -> Int -> State
updateState st n v = st { vars = updateVars n v (vars st) }

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
                  _ -> if inp == ":q" then putStrLn "Bye" else -- Quit command
                       if '!' `elem` inp then putStrLn $ show $ getCommand st n else -- Get the most recent command
                       if inp == "it" then putStrLn $ show it -- Show recent variable
                       else do putStrLn "Parse error"
                               repl st
                       where n = read $ drop 1 inp :: Int
