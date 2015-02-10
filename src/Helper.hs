module Helper where

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