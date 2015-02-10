module Helper where

import System.IO

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