{- LANGUAGE DatatypeContexts -}

module REPL where

import Expr
import Parsing
import Helper
import Data.List 
import Data.Maybe

data State = State { vars     :: Tree (Name, Float),
                     funcs    :: Tree (Name, [String]),
                     numCalcs :: Int,
                     history  :: [Command],
                     commands :: [String] }

initState :: State
initState = State Empty Empty 0 [] []

updateTreeVars :: (Ord a) => a -> b -> Tree (a, b) -> Tree (a, b)
updateTreeVars n v Empty = Node (n,v) Empty Empty
updateTreeVars n v (Node (x,y) t1 t2) | n < x = Node (x,y) (updateTreeVars n v t1) t2
                                      | n > x = Node (x,y) t1 (updateTreeVars n v t2)
                                      | otherwise = Node (n,v) t1 t2

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- Get the most recent command from the history
getHistory :: State -> Int -> Command
getHistory st n = history st !! n

-- Update variable key-value pairs list
--addVar :: State -> a -> b -> State
--addVar st n v = st { vars = updateTreeVars n v (vars st) }

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
                      _:xs -> st { commands = xs }
					  
-- Handles the errors of loading a file
load :: [String] -> State -> IO ()
load cmd st = do case (length cmd /= 2) of
                    True -> do putStrLn "Usage: :l <filename>"
                               repl st
                    False -> do contents <- loadFile (cmd !! 1)
                                let st' = addCommands st (wordsWhen (=='\n') contents) in
                                  repl st'

-- Execute a program command
exec :: [String] -> State -> IO ()
exec cmd st = case head cmd of
                ":q" -> putStrLn "Bye"
                ":l" -> load cmd st
                _    -> do putStrLn $ "\"" ++ head cmd ++ "\" not recognized command"
                           repl st

readLine :: State -> IO String
readLine st | hasCommands st  = let cmd = (commands st) !! 0 in
                                    do putStrLn $ cmd
                                       return cmd
            | otherwise       = getLine

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' x = (addHistory st (Set var e)) { vars = updateTreeVars var x (vars st) } 
              in case eval (vars st) e of
                  Right a  -> do putStrLn "OK!"
                                 repl $ st' a
                  Left err -> do printErr err
                                 repl $ st
          -- st' should include the variable set to the result of evaluating e

process st (Eval e) 
     = do let ev  = eval (vars st) e
          let st' = st { vars = updateTreeVars "it" (fromRight ev) (vars ((addHistory st (Eval e)) {numCalcs = numCalcs st + 1}))} 
              in case ev of
                Right x  -> do case isInt x of
                                True -> putStrLn $ show $ truncate x
                                False -> putStrLn $ show $ x
                               repl st'
                Left err -> do putStrLn err
                               repl st

process st (Print i s)
     = do if i == "echo" then putStrLn $ show $ s
          else putStrLn $ "Unrecognized print command"
          repl $ st

process st (Loop i n e)
     = do if i == "loop" then do let st' = addCommands st (replicate n e) in
                                     repl $ st'
          else do putStrLn $ "Usage: loop <n-times> <expression>"
                  repl $ st

process st (FunctionInit f n e)
     = do let st' = st { funcs = updateTreeVars n [e] (funcs st)}
          if f == "function" then repl $ st'                       
          else repl $ st
process st (FunctionCall f)
     = do let val = getValueFromTree f (funcs st)
              --ev = fromRight(eval (Empty) (fromJust(val)))  
              st' = addCommands st (fromJust $ val) in
              repl st'   

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- readLine st
             let st' = removeCommand st in -- remove command from list if file was used
               case parse pCommand inp of
                  [(cmd, "")] -> process st' cmd -- Must parse entire input
                  _ -> if (isPrefixOf ":" inp) && (length inp >= 2) then exec (words inp) st' -- Execute program command (e.g. :q - quit)
                            else if '!' `elem` inp then process st' (getHistory st' n) -- Get the most recent command from history
                                else do printErr $ "Could not parse \"" ++ inp ++ "\""
                                        repl st'
                            where n = read $ drop 1 inp :: Int
