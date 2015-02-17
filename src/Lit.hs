module Lit where

data Lit = ILit Int | FLit Float
    deriving (Show, Eq, Read, Ord)

add :: Lit -> Lit -> Lit
add (ILit a) (ILit b) = ILit (a + b)
add (FLit a) (FLit b) = FLit (a + b)
add (ILit a) (FLit b) = FLit ((fromIntegral a) + b)
add (FLit a) (ILit b) = FLit (a + (fromIntegral b))

sub :: Lit -> Lit -> Lit
sub (ILit a) (ILit b) = ILit (a - b)
sub (FLit a) (FLit b) = FLit (a - b)
sub (ILit a) (FLit b) = FLit ((fromIntegral a) - b)
sub (FLit a) (ILit b) = FLit (a - (fromIntegral b))

mul :: Lit -> Lit -> Lit
mul (ILit a) (ILit b) = ILit (a * b)
mul (FLit a) (FLit b) = FLit (a * b)
mul (ILit a) (FLit b) = FLit ((fromIntegral a) * b)
mul (FLit a) (ILit b) = FLit (a * (fromIntegral b))

div' :: Lit -> Lit -> Lit
div' (ILit a) (ILit b) = ILit (a `div` b)
div' (FLit a) (FLit b) = FLit (a / b)
div' (ILit a) (FLit b) = FLit ((fromIntegral a) / b)
div' (FLit a) (ILit b) = FLit (a / (fromIntegral b))

abs' :: Lit -> Lit
abs' (ILit x) | x < 0		= ILit $ -x
			  | otherwise 	= ILit x
abs' (FLit x) | x < 0		= FLit $ -x
			  | otherwise 	= FLit x

pow :: Lit -> Lit -> Lit
pow (ILit a) (ILit b) = case b < 0 of 
							True -> FLit ((fromIntegral a) ** (fromIntegral b))
							False -> ILit (a ^ b)
pow (FLit a) (FLit b) = FLit (a ** b)
pow (ILit a) (FLit b) = FLit ((fromIntegral a) ** b)
pow (FLit a) (ILit b) = FLit (a ** (fromIntegral b))

mod' :: Lit -> Lit -> Lit
mod' (ILit a) (ILit b) = ILit (a `mod` b)