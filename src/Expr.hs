module Expr where

import Parsing
import Helper

<<<<<<< Updated upstream
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Val Float
          | Name Name
          | Err String
          | Abs Expr
          | Mod Expr Expr
          | Power Expr Expr
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | Print Name Name
	         | Simp Name Expr
             | Loop Name Int Name
             | FunctionInit Name Name Name
             | FunctionCall Name
             | Quit Name
  deriving Show

eval :: Tree (Name, Float) -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either String Float -- Result (if no errors such as missing variables)

eval _    (Val x)     = Right x
eval vars (Add x y)   = val (+) vars x y
eval vars (Sub x y)   = val (-) vars x y
eval vars (Mul x y)   = val (*) vars x y
eval vars (Div x y)   = val (/) vars x y
eval _    (Err msg)   = Left msg
eval vars (Abs n)     = Right (abs (fromRight (eval vars n)))
eval vars (Power x y) = Right ((fromRight (eval vars x)) ** (fromRight (eval vars y)))
=======
-- | These are Expressions used in Command
data Expr = Add Expr Expr   -- ^ Addition
          | Sub Expr Expr   -- ^ Subtration
          | Mul Expr Expr   -- ^ Multiplication
          | Div Expr Expr   -- ^ Division
          | Val Float       -- ^ Single number
          | Name Name       -- ^ Single identifier
          | Err String      -- ^ Error message
		      | Abs Expr        -- ^ Ablosute value
          | Mod Expr Expr   -- ^ Modulo
          | Power Expr Expr -- ^ Power
  deriving Show

-- | These are the REPL commands 
data Command = Set Name Expr                -- ^ Setting expression to the variable
             | Eval Expr                    -- ^ Expression evaluation
             | Print Name Name              -- ^ Printing the expression
             | Loop Name Int Name           -- ^ Looping, the number of times and expression as a string
             | FunctionInit Name Name Name  -- ^ Initialize the function, function name, expression as a string 
             | FunctionCall Name            -- ^ Calling function
  deriving Show

-- | Evaluation function evaluates a given expression and produces the result 
eval :: Tree (Name, Float) -> -- ^ Variable name to value mapping
        Expr ->               -- ^ Expression to evaluate
        Either String Float   -- ^ Result 
eval _    (Val x)   = Right x          -- ^ Single number
eval vars (Add x y) = val (+) vars x y -- ^ Addition
eval vars (Sub x y) = val (-) vars x y -- ^ Subtraction
eval vars (Mul x y) = val (*) vars x y -- ^ Multiplication
eval vars (Div x y) = val (/) vars x y -- ^ Division
eval _    (Err msg) = Left msg         -- ^ Error message
eval vars (Name x)  = 
  case getValueFromTree x vars of                         -- ^ if a name is in the tree
    Just a  -> Right a                                    -- ^ single variable
    Nothing -> Left "Use of undeclared variable"          -- ^ error message
eval vars (Abs n) = Right (abs (fromRight (eval vars n))) -- ^ absolute value
eval vars (Mod x y) = 
  case isInt (fromRight (eval vars x)) && isInt (fromRight (eval vars y)) of            -- ^ checks that both number are the integers
		True -> Right $ fromIntegral $ (toInteger $ round $ fromRight $ eval vars x)        -- ^ converts float to the int fro the first value
            `mod` (toInteger $ round $ fromRight $ eval vars y)                         -- ^ converts float to the int fro the second value
		False -> Left "Can't mod floats"                                                    -- ^ error message				
eval vars (Power x y) = Right ((fromRight (eval vars x)) ** (fromRight (eval vars y)))  -- ^ raises one value to the power of another value
>>>>>>> Stashed changes

eval vars (Mod x y)
    = case isInt (fromRight (eval vars x)) && isInt (fromRight (eval vars y)) of
        True -> Right $ fromIntegral $ (toInteger $ round $ fromRight $ eval vars x) `mod` (toInteger $ round $ fromRight $ eval vars y)
        False -> Left "Can't mod floats"

eval vars (Name x)  = case getValueFromTree x vars of
                        Just a  -> Right a
                        Nothing -> Left "Use of undeclared variable"

-- | apply an operator to the evalution of the left and right expressions       
val :: Ord a => (Float -> Float -> Float) -> Tree (a, b) -> Expr -> Expr -> Either String Float
val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)

pCommand :: Parser Command
pCommand = do l <- letter
              symbol "="
              e <- pExpr
              return (Set [l] e)
            ||| do i <- identifier
                   symbol "$"
                   s <- anything
                   return (Print i s)
                ||| do i <- identifier
                       n <- natural
                       e <- anything
                       return (Loop i n e)
                     ||| do symbol ":"
                            i <- identifier
                            e <- pExpr
                            return (Simp i e)
                          ||| do f <- identifier --"function"
                                 n <- identifier -- function name 
                                 symbol "():" -- add list of arguments
                                 e <- anything
                                 return (FunctionInit f n e)
                               ||| do n <- identifier -- function name 
                                      symbol "()" -- add list of arguments
                                      return (FunctionCall n)
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
              ||| do symbol "|"
                     e <- pExpr
                     symbol "|"
                     return (Abs e)
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
                 ||| do symbol "^"
                        e <- pTerm
                        return (Power f e)
                    ||| do symbol "mod"
                           e <- pTerm
                           return (Mod f e) 
                        ||| return f
