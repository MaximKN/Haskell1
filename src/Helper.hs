module Helper where

import System.IO
import Lit
----------------------- BINARY TREES ------------------------
-- | Alias for variable name
type Name = String

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

-- | Print Lit values to the console
echo :: Lit -> IO ()
echo x = case x of
            ILit x -> print x
            FLit x -> print x

-- | Print Lit values to the console
echoString :: Lit -> String
echoString x = case x of
            ILit x -> show x
            FLit x -> show x

-- | Similar to function word, except it takes a predicate
-- | Src: http://stackoverflow.com/a/4981265/2849447
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
-- | The 'fromRight' function extracts the element out of a 'Right' and
-- | throws an error if its argument take the form  @Left _@.
-- | Src: https://hackage.haskell.org/package/either-unwrap-1.1/docs/src/Data-Either-Unwrap.html#isRight
fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x
