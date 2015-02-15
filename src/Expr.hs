module Expr where

import Parsing
import Helper

-- | These are Expressions used in Command
data Expr = Add Expr Expr   -- ^ Addition
          | Sub Expr Expr   -- ^ Subtration
          | Mul Expr Expr   -- ^ Multiplication
          | Div Expr Expr   -- ^ Division
          | Val Float       -- ^ Single number
          | Name Name       -- ^ Single identifier
          | Err String      -- ^ Error message
          | Abs Expr        -- ^ Absolute value
          | Mod Expr Expr   -- ^ Modulo
          | Power Expr Expr -- ^ Power
  deriving Show

-- | These are the REPL commands
data Command = Set Name Expr                -- ^ Setting expression to the variable
             | Eval Expr                    -- ^ Expression evaluation
             | Print String                 -- ^ Printing the expression
             | Loop Int String              -- ^ Looping, the number of times and expression as a string
             | FunctionInit Name String     -- ^ Initialize the function, function name, expression as a string 
             | FunctionCall Name            -- ^ Calling function
             | Simplify Expr
  deriving Show

-- | Evaluation function evaluates a given expression and produces the result 
eval :: Tree (Name, Float) -> -- ^ Variable name to value mapping
        Expr ->               -- ^ Expression to evaluate
        Either String Float   -- ^ Result 

eval _    (Val x)     = Right x                         -- ^ Single number
eval vars (Add x y)   = val (+) vars x y                -- ^ Addition
eval vars (Sub x y)   = val (-) vars x y                -- ^ Subtraction
eval vars (Mul x y)   = val (*) vars x y                -- ^ Multiplication
eval vars (Div x y)   = val (/) vars x y                -- ^ Division
eval _    (Err msg)   = Left  $ msg                     -- ^ Error message
eval vars (Abs x)     = Right $ abs (fromRight (eval vars x))   -- ^ absolute value
eval vars (Power x y) = Right $ (fromRight (eval vars x))       -- ^ raises one value to the power of another value
                               ** (fromRight (eval vars y))

eval vars (Name x) 
    = case getValueFromTree x vars of                   -- ^ if a name is in the tree
        Just a  -> Right a                              -- ^ single variable
        Nothing -> Left "Use of undeclared variable"    -- ^ error message
    
eval vars (Mod x y)
    =  case isInt (fromRight (eval vars x)) && isInt (fromRight (eval vars y)) of    -- ^ checks that both number are the integers
		    True -> Right $ fromIntegral $ (toInteger $ round $ fromRight $ eval vars x) -- ^ converts float to the int fro the first value
                                 `mod` (toInteger $ round $ fromRight $ eval vars y) -- ^ converts float to the int fro the second value
		    False -> Left "Can't mod floats"                                             -- ^ error message				

-- | Apply an operator to the evalution of the left and right expressions       
val :: (Float -> Float -> Float)     -- ^ Operator function
              -> Tree (Name, Float)  -- ^ Tree of variables
              -> Expr                -- ^ Left expression
              -> Expr                -- ^ Right expression
              -> Either String Float -- ^ Result
val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)

-- | Parse commands
pCommand :: Parser Command
pCommand = do l <- letter -- ^ Variable assignment
              symbol "="
              e <- pExpr
              return (Set [l] e)
            ||| do string "print" -- ^ Print statement
                   space
                   s <- anything
                   return (Print s)
                ||| do string "loop"  -- ^ Loop construct
                       n <- natural
                       e <- anything
                       return (Loop n e)
                     ||| do string ":simplify"  -- ^ Simplify function
                            e <- pExpr
                            return (Simplify e)
                          ||| do string "function" -- ^ Function declaration
                                 n <- identifier -- ^ Function identifier 
                                 symbol "():"
                                 e <- anything
                                 return (FunctionInit n e)
                               ||| do n <- identifier -- ^ Function call
                                      symbol "()"
                                      return (FunctionCall n)
                                   ||| do e <- pExpr
                                          return (Eval e)

-- | Parse expression
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do symbol "+" -- ^ Addition
              e <- pExpr
              return (Add t e)
            ||| do symbol "-"   -- ^ Subtraction
                   e <- pExpr
                   return (Sub t e) 
                 ||| return t

-- | Parse factors
pFactor :: Parser Expr
pFactor = do d <- float     -- ^ Literal
             return (Val d)
          ||| do v <- identifier    -- ^ Variable
                 return (Name v)
              ||| do symbol "|"     -- ^ Absolute value
                     e <- pExpr
                     symbol "|"
                     return (Abs e)
                  ||| do symbol "(" -- ^ Bracketed expression
                         e <- pExpr
                         symbol ")"
                         return e
                      ||| return (Err ("Could not parse expression"))

-- | Parse terms
pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"    -- ^ Multiplication
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"   -- ^ Division
                   t <- pTerm
                   return (Div f t)
                 ||| do symbol "^"  -- ^ Exponential
                        e <- pTerm
                        return (Power f e)
                    ||| do symbol "mod" -- ^ Modulo
                           e <- pTerm
                           return (Mod f e) 
                        ||| return f
