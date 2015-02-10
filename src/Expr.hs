module Expr where

import Parsing

type Name = String
type Msg  = String

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Val Int
          | Name Name
          | Err Msg
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
  deriving Show

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
-- Either String Int

eval _    (Val x)   = Just x -- for values, just give the value directly
eval vars (Add x y) = val (+)   vars x y
eval vars (Sub x y) = val (-)   vars x y
eval vars (Mul x y) = val (*)   vars x y
eval vars (Div x y) = val (div) vars x y
--eval _    (Err msg) = Left msg
eval vars (Name x)  = case getIdent x vars of
                        Just a  -> Just a
                        Nothing -> Nothing --Left "Variable undefined"
eval _ _ = Nothing -- catch errors

--EDIT
--val :: (a -> b -> a) -> [(Name, Int)] -> Expr -> Expr -> IO (Int)
val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)

-- Get value from the state by name
getIdent :: Name -> [(Name, Int)] -> Maybe Int
getIdent _ [] = Nothing
getIdent n ((x,y):xs) | n == x = Just y
                      | otherwise = getIdent n xs

pCommand :: Parser Command
pCommand = do t <- letter
              symbol "="
              e <- pExpr
              return (Set [t] e)
            ||| do e <- pExpr
                   return (Eval e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do symbol "+"
              e <- pExpr
              return (Add t e)
            ||| do symbol "-"
                   e <- pExpr
                   return (Sub t e) 
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- integer
             return (Val d)
           ||| do v <- identifier
                  return (Name v)
                ||| do symbol "("
                       e <- pExpr
                       symbol ")"
                       return e
                     ||| do err <- pErr
                            return err

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| return f

pErr :: Parser Expr
pErr = do s <- letter
          return (Err "Internal error")
        ||| return (Err "Unrecognized characters")
