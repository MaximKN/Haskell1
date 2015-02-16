module Helper where

import System.IO

data Lit = ILit Int
         | FLit Float
         | SLit String
  deriving Show

instance Num Lit where
--  (+) (ILit x) (ILit y) = x + y
  (+) (FLit x) (FLit y) = FLit (x + y)
  --(+) (ILit x) (FLit y) = FLit (x + y)
  --(+) (FLit x) (ILit y) = FLit (x + y)
  (+) (SLit x) (SLit y) = SLit ((show x) ++ (show y))
  --(+) (ILit x) (FLit y) = FLit (x + y)

  (*) (ILit x) (ILit y) = ILit (x * y)
  (*) (FLit x) (FLit y) = FLit (x * y)
  --(*) (ILit x) (FLit y) = FLit (x * y)
  --(*) (FLit x) (ILit y) = FLit (x * y)

instance Fractional Lit where
  (/) (FLit x) (FLit y)  = FLit (x / y)

{-}
instance Floating Lit where
  (**) (Flit x) (Flit y) = FLit (x ** y)
-}
{-}
instance Integral Lit where
  (div) (ILit x) (ILit y) = ILit (x `div` y)
-}

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
{-process st (Simp i e)
	= do case simplify e of 
		     Left e -> do putStrLn $ show $ e
		                  repl $ st
             Right f -> case isInt f of
                                True -> do putStrLn $ show $ truncate f
                                           repl st'
                                False -> do putStrLn $ show $ f
                                            repl st'-}
------------------- END OF BINARY TREES -----------------------

-- check if a floating point number can be converted into an int
{-isInt   :: Lit a => a -> Bool
======
-- | Check if a floating point number can be converted into an int
isInt   :: RealFrac a => a -> Bool
>>>>>>> bfc8c004b28c1a31dff8a9c74b868fd1189064d5
isInt x = x == fromInteger (round x)
-}

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
