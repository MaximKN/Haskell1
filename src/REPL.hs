module REPL where

import Expr
import Parsing
import Helper
import Data.List 
import Simplify
import Data.Maybe
import Control.Exception

-- |Maintain state of program
data State = State { vars     :: Tree (Name, Float), -- Stores variables
                     funcs    :: Tree (Name, [String]), -- Stores functions and their definitions
                     numCalcs :: Int, -- Keeps track of the number of calculations
                     history  :: [Command], -- Keeps track of command history
                     commands :: [String] } -- List of commands to be executed

-- |Initialize state to default values
initState :: State
initState = State Empty Empty 0 [] []

-- |Add variable to tree of variables
updateTreeVars :: (Ord a) => a -> b -> Tree (a, b) -> Tree (a, b)
updateTreeVars n v Empty = Node (n,v) Empty Empty
updateTreeVars n v (Node (x,y) t1 t2) | n < x = Node (x,y) (updateTreeVars n v t1) t2
                                      | n > x = Node (x,y) t1 (updateTreeVars n v t2)
                                      | otherwise = Node (n,v) t1 t2

-- |Update variable key-value pairs list
addVar :: Name -> Float -> State -> State
addVar n v st = st { vars = updateTreeVars n v (vars st) }

-- |Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- |Get the most recent command from the history
getHistory :: State -> Int -> Either String Command
getHistory st n | n < 0                      = Left "Index less than zero! Please provide a positive index into the history."
                | n >= (length $ history st) = Left "Index too big! Please provide an appropriate index into the history."
                | otherwise                  = Right $ history st !! n
                
-- |Add commands to the list of commands to be executed
addCommands :: State -> [String] -> State
addCommands st cmds = st { commands = (cmds ++ (commands st)) }

-- |Returns true if the list of commands isn't empty
hasCommands :: State -> Bool
hasCommands st = length (commands st) /= 0

-- |Removes top-most command from list
removeCommand :: State -> State
removeCommand st = case commands st of
                      [] -> st
                      _:xs -> st { commands = xs }
					  
-- |Handles the errors of loading a file
load :: [String] -> State -> IO ()
load cmd st = case (length cmd /= 2) of
                    True -> do putStrLn "Usage: :l <filename>"
                               repl st
                    False -> do result <- try $ loadFile (cmd !! 1) :: IO (Either IOException String)
                                case result of 
                                  Left exception -> do putStrLn $ "Fault: " ++ show exception
                                                       repl st
                                  Right contents -> let st' = addCommands st (wordsWhen (=='\n') contents) in
                                                        repl st'

-- |Execute a program command
exec :: [String] -> State -> IO ()
exec cmd st = case head cmd of
                ":q" -> putStrLn "Bye"
                ":l" -> load cmd st
                _    -> do putStrLn $ "\"" ++ head cmd ++ "\" not recognized command"
                           repl st

-- |Get next command in the list of commands in state, otherwise read command from console
readLine :: State -> IO String
readLine st | hasCommands st  = let cmd = (commands st) !! 0 in
                                    do putStrLn cmd
                                       return   cmd
            | otherwise       = getLine

-- |Set a variable and update the state
process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' x = addVar var x $ addHistory st (Set var e)
              in case eval (vars st) e of
                  Right a  -> do putStrLn "OK"
                                 repl $ st' a
                  Left err -> do printErr err
                                 repl $ st

-- |Evaluate an expression and print result to the console                           
process st (Eval e)
     = do let ev  = eval (vars st) e
              in case ev of
                Right x  -> do case isInt x of
                                True -> putStrLn $ show $ truncate x
                                False -> putStrLn $ show $ x
                               repl $ addVar "it" (fromRight ev) $ (addHistory st $ Eval e) {numCalcs = numCalcs st + 1}
                Left err -> do putStrLn err
                               repl st

-- |Take an expression and simplify it
process st (Simplify e) = repl $ st
{-process st (Simp i e)
	= do case simplify e of 
		     Left e -> do putStrLn $ show $ e
		                  repl $ st
             Right f -> case isInt f of
                                True -> do putStrLn $ show $ truncate f
                                           repl st'
                                False -> do putStrLn $ show $ f
                                            repl st'-}
                                            
-- |Take a string and print it to the console
process st (Print s) = do putStrLn $ s
                          repl $ st

-- |Evaluate an expression n times
process st (Loop n e)
     = repl $ addCommands st (replicate n e)

-- |Define and store a function
process st (FunctionInit n e)
     = repl $ st { funcs = updateTreeVars n [e] (funcs st)}

-- |Add functions expressions to list of commands to get executed
process st (FunctionCall f)
     = let val = getValueFromTree f (funcs st) in
           repl $ addCommands st $ fromJust $ val

-- |Read, Eval, Print Loop
-- ^ This reads and parses the input using the pCommand parser, and calls
-- ^ 'process' to process the command.
-- ^ 'process' will call 'repl' when done, so the system loops.
repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- readLine st
             let st' = removeCommand st in -- remove command from list if file was used
               case parse pCommand inp of
                  [(cmd, "")] -> process st' cmd -- Must parse entire input
                  _ -> if (isPrefixOf ":" inp) && (length inp >= 2) then exec (words inp) st' -- Execute program command (e.g. :q - quit)
                            else if '!' `elem` inp then do case getHistory st' n of 
                                                            Left s -> putStrLn s
                                                            Right command -> process st' command -- Get the most recent command from history and execute it
                                                           repl st'
                                                       
                                else do printErr $ "Could not parse \"" ++ inp ++ "\""
                                        repl st'
                            where n = read $ drop 1 inp :: Int
