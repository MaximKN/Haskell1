module REPL where

import Expr
import Parsing
import Helper
import System.IO
import Data.List
import Data.Maybe
import Data.String

data State = State { vars     :: Tree (Name, Int),
                     numCalcs :: Int,
                     history  :: [Command],
                     commands :: [String] }

data Tree a = Empty 
            | Node a (Tree a) (Tree a) 
  deriving Show

initState :: State
initState = State Empty 0 [] []

updateTreeVars :: (Ord a, Ord b) => a -> b -> Tree (a, b) -> Tree (a, b)
updateTreeVars n v Empty = Node (n,v) Empty Empty
updateTreeVars n v (Node (x,y) t1 t2) | n < x = Node (x,y) (updateTreeVars n v t1) t2
                                      | n > x = Node (x,y) t1 (updateTreeVars n v t2)
                                      | otherwise = Node (n,v) t1 t2

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateListVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateListVars n v xs = (n,v):dropVar n xs

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar _ [] = []
dropVar n ((x,y):xs) | n == x = xs
                     | otherwise = (x,y):dropVar n xs

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- Get the most recent command from the history
getCommand :: State -> Int -> Command
getCommand st n = history st !! n

-- Get value from the list by name
getValueFromList :: Name -> [(Name, Int)] -> Int
getValueFromList _ [] = 0
getValueFromList n ((x,y):xs) | n == x = y
                              | otherwise = getValueFromList n xs

getValueFromTree :: Name -> Tree (Name, Int) -> Int
getValueFromTree _ Empty = 0
getValueFromTree n (Node (z,q) t1 t2) | n < z = getValueFromTree n t1
                                      | n > z = getValueFromTree n t2
                                      | otherwise = q

flatTree :: (Ord a, Ord b) => Tree (a, b) -> [(a, b)]
flatTree Empty = []
flatTree (Node x t1 t2) = flatTree t1 ++ [x] ++ flatTree t2

-- Update variable key-value pairs list
updateState :: State -> Name -> Int -> State
updateState st n v = st { vars = updateTreeVars n v (vars st) }

-- Print to the error stream (stderr)
printErr :: String -> IO ()
printErr err = hPutStrLn stderr err

-- Add commands to the commands list in the state
addCommands :: State -> [String] -> State
addCommands st cmds = st { commands = (cmds ++ (commands st)) }

-- Returns true if the list of commands isn't empty
hasCommands :: State -> Bool
hasCommands st = length (commands st) /= 0

-- Removes top-most command from list
removeCommand :: State -> State
removeCommand st = case commands st of
                      [] -> st
                      x:xs -> st { commands = xs }

-- Execute a program command
exec :: [String] -> State -> IO ()
exec cmd st = case head cmd of
                ":q" -> putStrLn "Bye"
                ":l" -> do case (length cmd /= 2) of
                            True -> do putStrLn "Usage: :l <filename>"
                                       repl st
                            False -> case cmd !! 1 of
                                  filename -> do contents <- loadFile filename
                                                 let st' = addCommands st (wordsWhen (=='\n') contents) in
                                                     repl st'
                _   -> do putStrLn $ "\"" ++ head cmd ++ "\" not recognized command"
                          repl st

readLine :: State -> IO String
readLine st | hasCommands st  = let cmd = (commands st) !! 0 in
                                  do putStrLn $ cmd
                                     return cmd
            | otherwise       = getLine

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let val = getValueFromTree var (vars st) -- Current value for the given variable 
              -- update the state by storing the variable and adding command to the history
              --REFACTOR
              st' x = updateState ((addHistory st (Set var e)) {numCalcs = numCalcs st + 1}) var x  
              in case eval [(var, val)] e of
                  Just a -> repl $ st' a
                  Nothing -> do printErr "Use of undeclared variable" --Left err -> do printErr err
                                repl $ st
          -- st' should include the variable set to the result of evaluating e
process st (Eval e) 
     = do let ev  = eval (flatTree (vars st)) e
     -- REFACTOR
          let st' = updateState ((addHistory st (Eval e)) {numCalcs = numCalcs st + 1}) "it" (fromJust ev)
              in case ev of
                Just x  -> do putStrLn $ show $ x --Right x  -> do putStrLn $ show $ x
                              repl st'
                Nothing -> do putStrLn "Error parsing expression" --Left err -> do printErr err
                              repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- readLine st
             let st' = removeCommand st in 
               case parse pCommand inp of
                  [(cmd, "")] -> process st' cmd -- Must parse entire input
                  _ -> if (isPrefixOf ":" inp) && (length inp >= 2) then exec (words inp) st' -- Execute program command (e.g. :q - quit)
                            else if '!' `elem` inp then process st' (getCommand st' n) -- Get the most recent command from history
                                else do printErr $ "Could not parse \"" ++ inp ++ "\""
                                        repl st'
                            where n = read $ drop 1 inp :: Int
