module Expr where

import Parsing

type Ident = String
type Name  = String

-- At first, 'Expr' contains only addition and values. You will need to 
-- add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Val Int
          | Ident Ident
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
  deriving Show

eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)

eval vars (Val x)   = Just x -- for values, just give the value directly
eval vars (Add x y) = val (+)   vars x y
eval vars (Sub x y) = val (-)   vars x y
eval vars (Mul x y) = val (*)   vars x y
eval vars (Div x y) = val (div) vars x y
eval vars (Ident x) = Just (getIdent x vars)

val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)

-- Get value from the state by name
getIdent :: Name -> [(Name, Int)] -> Int
getIdent _ [] = 0
getIdent n ((x,y):xs) | n == x = y
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
pFactor = do d <- natural
             return (Val d)
           ||| do v <- letter
                  return (Ident [v])
                ||| do symbol "("
                       e <- pExpr
                       symbol ")"
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| return f
