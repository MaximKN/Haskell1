module REPL where

import Expr
import Parsing
import Helper
import Simplify
import Data.Maybe
import Control.Exception

-- | Maintain state of program
data State = State { vars     :: Tree (Name, Lit),      -- Stores variables
                     funcs    :: Tree (Name, [String]), -- Stores functions and their definitions
                     numCalcs :: Int,                   -- Keeps track of the number of calculations
                     history  :: [Command],             -- Keeps track of command history
                     commands :: [String] }             -- List of commands to be executed

-- | Initialize state to default values
initState :: State
initState =  State Empty Empty 0 [] []

-- | Add variable to tree of variables
updateTreeVars :: (Ord a) => a -> b -> Tree (a, b) -> Tree (a, b)
updateTreeVars n v Empty  = Node (n,v) Empty Empty
updateTreeVars n v (Node (x,y) t1 t2) | n < x     = Node (x,y) (updateTreeVars n v t1) t2
                                      | n > x     = Node (x,y) t1 (updateTreeVars n v t2)
                                      | otherwise = Node (n,v) t1 t2

-- | Update variable key-value pairs list
addVar :: Name -> Lit -> State -> State
addVar n v st = st { vars = updateTreeVars n v (vars st) }

-- | Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd:history st }

-- | Get the most recent command from the history
getHistory :: State -> Int -> Either String Command
getHistory st n | n < 0                      = Left "Index less than zero! Please provide a positive index into the history."
                | n >= length (history st)   = Left "Index too big! Please provide an appropriate index into the history."
                | otherwise                  = Right $ history st !! n
                
-- | Add commands to the list of commands to be executed
addCommands :: State -> [String] -> State
addCommands st cmds = st { commands = cmds ++ commands st }

-- | Returns true if the list of commands isn't empty
hasCommands :: State -> Bool
hasCommands st = not (null (commands st))

-- | Removes top-most command from list
removeCommand :: State -> State
removeCommand st = case commands st of
                      [] -> st
                      _:xs -> st { commands = xs }

-- | Get next command in the list of commands in state, otherwise read command from console
readLine :: State -> IO String
readLine st | hasCommands st  = let cmd = head (commands st) in
                                    do putStrLn cmd
                                       return   cmd
            | otherwise       = getLine

-- | Print Lit values to the console
echo :: Lit -> IO()
echo x = case x of
            ILit x -> print x
            FLit x -> print x

-- | Set a variable and update the state
process :: State -> Command -> IO ()
process st (Set var e) 
     = let st' x = addVar var x $ addHistory st (Set var e)
              in case eval (vars st) e of
                  Right a  -> do putStrLn "OK"
                                 repl $ st' a
                  Left err -> do putStrLn err
                                 repl st

-- | Evaluate an expression and print result to the console                           
process st (Eval e)
     = let ev  = eval (vars st) e
              in case ev of
                Right x  -> do echo x
                               repl $ addVar "it" x $ (addHistory st $ Eval e) {numCalcs = numCalcs st + 1}
                Left err -> do putStrLn err
                               repl st

-- | Load file
process st (Load filename) = do contents <- try $ readFile filename :: IO (Either IOException String)
                                case contents of 
                                  Left exception -> do print exception
                                                       repl st
                                  Right contents -> let st' = addCommands st (wordsWhen (=='\n') contents) in
                                                        repl st'

-- | Quit application
process _ (Quit) = putStrLn "Bye!"

-- | Take an expression and simplify it
process st (Simplify e) = do case simplify e of 
                               Left err -> print err
                               Right e  -> print e
                             repl st
                                            
-- | Take a string and print it to the console
process st (Print s) = do putStrLn s
                          repl st

-- | Evaluate an expression n times
process st (Loop n e)
     = repl $ addCommands st (replicate n e)

-- | Define and store a function
process st (FunctionInit n e)
     = repl $ st { funcs = updateTreeVars n [e] (funcs st)}

-- | Add functions expressions to list of commands to get executed
process st (FunctionCall f)
     = let val = getValueFromTree f (funcs st) in
           repl $ addCommands st $ fromJust val

-- | Index into command history and execute select command
process st (History index)
    = do case getHistory st index of 
              Left s -> putStrLn s
              Right command -> process st command -- Get the most recent command from history and execute it
         repl st

-- | Read, Eval, Print Loop
-- ^ This reads and parses the input using the pCommand parser, and calls
-- ^ 'process' to process the command.
-- ^ 'process' will call 'repl' when done, so the system loops.
repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- readLine st
             let st' = removeCommand st in -- remove command from list if file was used
               case parse pCommand inp of
                  [(cmd, "")] -> process st' cmd -- Must parse entire input
                  _ -> do putStrLn $ "Error: could not parse \"" ++ inp ++ "\"."
                          repl st'
                          
