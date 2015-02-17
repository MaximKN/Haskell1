module Expr where

import Parsing
import Helper
import Lit

-- | These are Expressions used in Command
data Expr = Add Expr Expr   -- ^ Addition
          | Sub Expr Expr   -- ^ Subtration
          | Mul Expr Expr   -- ^ Multiplication
          | Div Expr Expr   -- ^ Division
          | Val Lit         -- ^ Single number
          | Ident Name      -- ^ Single identifier
          | Abs Expr        -- ^ Absolute value
          | Mod Expr Expr   -- ^ Modulo
          | Power Expr Expr -- ^ Power
  deriving Show

-- | These are the REPL commands
data Command = Set Name Expr                -- ^ Setting expression to the variable
             | Eval Expr                    -- ^ Expression evaluation
             | Print Expr                   -- ^ Printing the expression
             | Loop Int String              -- ^ Looping, the number of times and expression as a string
             | FunctionInit Name String     -- ^ Initialize the function, function name, expression as a string 
             | FunctionCall Name            -- ^ Calling function
             | Simplify Expr                -- ^ Simplify an expression
             | Load Name                    -- ^ Load a file and execute its commands
             | History Int                  -- ^ Index into history and execute selected command
             | Help                         -- ^ Display how to use certain commands
             | Quit                         -- ^ Quit the application
  deriving Show

-- | Evaluation function evaluates a given expression and produces the result 
eval :: Tree (Name, Lit)         -- ^ Variable name to value mapping
        -> Expr                  -- ^ Expression to evaluate
        -> Either Msg Lit        -- ^ Error message or numeric result 

-- Basic arithmetic operations for eval
eval vars (Val x)   = Right x	
eval vars (Add x y) = val add vars x y
eval vars (Sub x y) = val sub vars x y
eval vars (Mul x y) = val mul vars x y
eval vars (Div x y) = val div' vars x y
						
-- More complex operations for eval
eval vars (Abs x)   = do x <- eval vars x
                         return $ abs' x
						 
eval vars (Power x y) = do x <- eval vars x
                           y <- eval vars y
                           return (pow x y)

eval vars (Ident x)   = case getValueFromTree x vars of
                            Just a  -> Right a
                            Nothing -> Left "Use of undeclared variable"

eval vars (Mod x y) = do x <- eval vars x
                         y <- eval vars y
                         return (mod' x y)

-- | Apply an operator to the evaluation of the left and right expressions       
val :: (Lit -> Lit -> Lit)           -- ^ Operator function
              -> Tree (Name, Lit)    -- ^ Tree of variables
              -> Expr                -- ^ Left expression
              -> Expr                -- ^ Right expression
              -> Either Msg Lit      -- ^ Error message or numeric result 

val op vars x y = do x <- eval vars x
                     y <- eval vars y
                     return (op x y)
                     
-- | Parse commands
pCommand :: Parser Command
pCommand = do symbol ":"
              do char 'q' -- Quit command
                 return Quit
               ||| do char 'l' -- Load command
                      space
                      filename <- anything
                      return (Load filename)
                    ||| do char 's'  -- Simplify function
                           space
                           e <- pExpr
                           return (Simplify e)
                         ||| do char 'h'
                                return (Help)
            ||| do char '!'
                   index <- int
                   return (History index)
                 ||| do l <- identifier -- Variable assignment
                        symbol "="
                        e <- pExpr
                        return (Set l e)
                      ||| do string "print" -- Print statement
                             space
                             s <- pExpr
                             return (Print s)
                           ||| do string "loop"  -- Loop construct
                                  n <- natural
                                  s <- anything
                                  return (Loop n s)
                                ||| do string "function" -- Function declaration
                                       n <- identifier -- Function identifier 
                                       symbol "():"
                                       e <- anything
                                       return (FunctionInit n e)
                                     ||| do n <- identifier -- Function call
                                            symbol "()"
                                            return (FunctionCall n)
                                          ||| do e <- pExpr
                                                 return (Eval e)
                                                 
-- | Parse expression
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do symbol "+" -- Addition
              e <- pExpr
              return (Add t e)
            ||| do symbol "-"   -- Subtraction
                   e <- pExpr
                   return (Sub t e) 
                 ||| return t

-- | Parse factors
pFactor :: Parser Expr
pFactor = do d <- floatIntStr   -- Literal
             return (Val d)
          ||| do v <- identifier    -- Variable
                 return (Ident v)
              ||| do symbol "|"     -- Absolute value
                     e <- pExpr
                     symbol "|"
                     return (Abs e)
                  ||| do symbol "(" -- Bracketed expression
                         e <- pExpr
                         symbol ")"
                         return e

-- | Parse terms
pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"    -- Multiplication
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"   -- Division
                   t <- pTerm
                   return (Div f t)
                 ||| do symbol "^"  -- Exponential
                        e <- pTerm
                        return (Power f e)
                    ||| do symbol "mod" -- Modulo
                           e <- pTerm
                           return (Mod f e) 
                        ||| return f
