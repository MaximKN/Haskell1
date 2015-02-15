module REPL where

import Expr
import Parsing
import Helper
import Data.List 
import Simplify
import Data.Maybe

-- | Maintain state of program
data State = State { vars     :: Tree (Name, Float), -- ^ Stores variables
                     funcs    :: Tree (Name, [String]), -- ^ Stores functions and their definitions
                     numCalcs :: Int, -- ^ Keeps track of the number of calculations
                     history  :: [Command], -- ^ Keeps track of command history
                     commands :: [String] } -- ^ List of commands to be executed

-- | Initialize state to default values
initState :: State
initState = State Empty Empty 0 [] []

-- | Add variable to tree of variables
updateTreeVars :: (Ord a) => a -> b -> Tree (a, b) -> Tree (a, b)
updateTreeVars n v Empty = Node (n,v) Empty Empty
updateTreeVars n v (Node (x,y) t1 t2) | n < x = Node (x,y) (updateTreeVars n v t1) t2
                                      | n > x = Node (x,y) t1 (updateTreeVars n v t2)
                                      | otherwise = Node (n,v) t1 t2

-- | Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- | Get the most recent command from the history
getHistory :: State -> Int -> Command
getHistory st n = history st !! n

-- | Add commands to the list of commands to be executed
addCommands :: State -> [String] -> State
addCommands st cmds = st { commands = (cmds ++ (commands st)) }

-- | Returns true if the list of commands isn't empty
hasCommands :: State -> Bool
hasCommands st = length (commands st) /= 0

-- | Removes top-most command from list
removeCommand :: State -> State
removeCommand st = case commands st of
                      [] -> st
                      _:xs -> st { commands = xs }
					  
-- | Handles the errors of loading a file
load :: [String] -> State -> IO ()
load cmd st = do case (length cmd /= 2) of
                    True -> do putStrLn "Usage: :l <filename>"
                               repl st
                    False -> do contents <- loadFile (cmd !! 1)
                                let st' = addCommands st (wordsWhen (=='\n') contents) in
                                  repl st'

-- | Execute a program command
exec :: [String] -> State -> IO ()
exec cmd st = case head cmd of
                ":q" -> putStrLn "Bye"
                ":l" -> load cmd st
                _    -> do putStrLn $ "\"" ++ head cmd ++ "\" not recognized command"
                           repl st

-- | Get next command in the list of commands in state, otherwise read command from console
readLine :: State -> IO String
readLine st | hasCommands st  = let cmd = (commands st) !! 0 in
                                    do putStrLn $ cmd
                                       return cmd
            | otherwise       = getLine


-- | Adds Name and Float to the tree
updateState :: Name -> Float -> State -> State
updateState n v st = st { vars = updateTreeVars n v (vars st) }

-- | Set a variable and update the state
process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' x = updateState var x $ addHistory st $ Set var e
              in case eval (vars st) e of
                  Right x  -> do putStrLn "OK"
                                 repl $ st' x
                  Left err -> do printErr err
                                 repl $ st

-- | Evaluate an expression and print result to the console                           
process st (Eval e)
     = do let ev  = eval (vars st) e
          let st' = updateState "it" (fromRight ev) $ (addHistory st $ Eval e) {numCalcs = numCalcs st + 1}
              in case ev of
                Right x  -> do case isInt x of
                                True -> putStrLn $ show $ truncate x
                                False -> putStrLn $ show $ x
                               repl st'
                Left err -> do putStrLn err
                               repl st

-- | Take an expression and simplify it
process st (Simp i e)
	= do if i == "simplify" then
		do case simplify e of 
		     Left e -> do putStrLn $ show $ e
                                  repl $ st
		     Right f -> do case isInt f of
					True -> putStrLn $ show $ truncate f
					False -> putStrLn $ show $ f
				   repl $ st
	      else repl $ st

-- | Take a string and print it to the console
process st (Print i s)
     = do if i == "echo" then
	     do putStrLn $ show $ s
	        repl $ st
           else repl $ st

-- | Evaluate an expression n times
process st (Loop i n e)
     = do if i == "loop" then do let st' = addCommands st (replicate n e) in
                                     repl $ st'
          else repl $ st

-- | Define and store a function
process st (FunctionInit f n e)
     = do let st' = st { funcs = updateTreeVars n [e] (funcs st)}
          if f == "function" then repl $ st'                       
          else repl $ st

-- Add functions expressions to list of commands to get executed
process st (FunctionCall f)
     = do let val = getValueFromTree f (funcs st)
              st' = addCommands st (fromJust $ val) in
              repl st'    

-- | Read, Eval, Print Loop
-- ^ This reads and parses the input using the pCommand parser, and calls
-- ^ 'process' to process the command.
-- ^ 'process' will call 'repl' when done, so the system loops.
repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- readLine st
             let st' = removeCommand st in -- ^ remove command from list if file was used
               case parse pCommand inp of
                  [(cmd, "")] -> process st' cmd -- ^ Must parse entire input
                  _ -> if (isPrefixOf ":" inp) && (length inp >= 2) then exec (words inp) st' -- ^ Execute program command (e.g. :q - quit)
                            else if '!' `elem` inp then process st' (getHistory st' n) -- ^ Get the most recent command from history
                                else do printErr $ "Could not parse \"" ++ inp ++ "\""
                                        repl st'
                            where n = read $ drop 1 inp :: Int
