module Simplify where
import Expr
import Helper

simplify :: Expr -> Either String Lit
simplify = simp

simp :: Expr -> Either String Lit
simp (Add x y) = if areFloats (Add x y) then
					Right (fromRight (eval Empty (Add x y)))
		 		 else case eval Empty x of
						Left s -> Left "x"
						Right v -> case eval Empty y of
										Left ss  -> Left $ show v ++ "+" ++ "x"
										Right vv -> Left $ show v ++ "+" ++ show vv 

			-- try and evaluate the left and right side as much as possible
			-- then concatenate results
			--Left $ (show (fromRight (eval (Empty) e))) ++ (stringToSymbol e) ++ (show (fromRight (eval (Empty) e)))

-- function to convert variable expression to variable name
getVarName :: Expr -> String
getVarName (Name x) = x

-- function to convert expression name to symbol
stringToSymbol :: Expr -> String
stringToSymbol (Add x y) = show $ show x ++ "+" ++ show y

-- function that returns true if operand is ident
isIdent :: Expr -> Bool
isIdent (Name x) = True
isIdent _        = False

-- function to decide if x or y is a variable
areIdents :: Expr -> Bool
areIdents e = not $ areFloats e

-- function that returns true if both operands are floats
areFloats :: Expr -> Bool
areFloats e = case eval Empty e of
				Right v -> True
				Left  s -> False

-- function that returns true if operand is float
isFloat :: Expr -> Bool
isFloat (Val x) = True
isFloat _       = False
