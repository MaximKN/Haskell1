module Expr where

import Parsing
import Helper

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Val Float
          | Name Name
          | Err String
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
  deriving Show

eval :: Tree (Name, Float) -> -- Variable name to value mapping
       Expr -> -- Expression to evaluate
        Either String Float -- Result (if no errors such as missing variables)

eval _    (Val x)   | isInt x   = Right (toInt x)
                    | otherwise = Right x
eval vars (Add x y) = val (+)   vars x y
eval vars (Sub x y) = val (-)   vars x y
eval vars (Mul x y) = val (*)   vars x y
eval vars (Div x y) = val (/) vars x y
eval _    (Err msg) = Left msg
eval vars (Name x)  = case getValueFromTree x vars of
                        Just a  -> Right a
                        Nothing -> Left "Use of undeclared variable"
eval _ _ = Left "Couldn't evaluate expression" -- catch errors

val :: (Float -> Float -> Float) -> Tree (Name, Float) -> Expr -> Expr -> Either String Float
val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)

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
pFactor = do d <- float
             return (Val d)
           ||| do v <- identifier
                  return (Name v)
                ||| do symbol "("
                       e <- pExpr
                       symbol ")"
                       return e
                     ||| return (Err ("Could not parse expression"))

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| return f
