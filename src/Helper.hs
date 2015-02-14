module Helper where

import System.IO

------------------- BINARY TREES -----------------------
type Name = String

data Tree a = Empty 
            | Node a (Tree a) (Tree a) 
  deriving Show

getValueFromTree :: Name -> Tree (Name, Int) -> Maybe Int
getValueFromTree _ Empty = Nothing
getValueFromTree n (Node (z,q) t1 t2) | n < z = getValueFromTree n t1
                                      | n > z = getValueFromTree n t2
                                      | otherwise = Just q





-- Similar to function word, except it takes a predicate
-- Src: http://stackoverflow.com/a/4981265/2849447
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
-- Read file from memory
loadFile :: String -> IO String
loadFile filename = readFile filename

-- Print to the error stream (stderr)
printErr :: String -> IO ()
printErr err = hPutStrLn stderr err

-- Src: https://hackage.haskell.org/package/either-unwrap-1.1/docs/src/Data-Either-Unwrap.html#isRight
-- | The 'fromRight' function extracts the element out of a 'Right' and
-- throws an error if its argument take the form  @Left _@.
fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x


----------------------- USED FUNCTIONS ------------------------

-- Get value from list of vars
getIdent :: Name -> [(Name, Int)] -> Maybe Int
getIdent _ [] = Nothing
getIdent n ((x,y):xs) | n == x = Just y
                      | otherwise = getIdent n xs

-- Get value from the list by name
getValueFromList :: Name -> [(Name, Int)] -> Int
getValueFromList _ [] = 0
getValueFromList n ((x,y):xs) | n == x = y
                              | otherwise = getValueFromList n xs

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

flatTree :: (Ord a, Ord b) => Tree (a, b) -> [(a, b)]
flatTree Empty = []
flatTree (Node x t1 t2) = flatTree t1 ++ [x] ++ flatTree t2