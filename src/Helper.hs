module Helper where

import System.IO
import Lit

type Name  = String  -- variable name
type Msg   = String  -- message

----------------------- BINARY TREES ------------------------
-- | Binary search tree
data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving Show

-- | Gets value from tree using key from key-pair to traverse tree
getValueFromTree :: (Ord a) => a -> Tree (a, b) -> Maybe b
getValueFromTree _ Empty = Nothing
getValueFromTree n (Node (z,q) t1 t2) | n < z = getValueFromTree n t1
                                      | n > z = getValueFromTree n t2
                                      | otherwise = Just q
------------------- END OF BINARY TREES -----------------------
-- | Similar to function word, except it takes a predicate
-- | Src: http://stackoverflow.com/a/4981265/2849447
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
