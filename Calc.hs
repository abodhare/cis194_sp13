module Calc where

import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit x
        | x <= 0 = False
        | otherwise = True
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    add (MinMax x) (MinMax y) = MinMax (max x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    mul (Mod7 x) (Mod7 y) = lit (x * y)
    add (Mod7 x) (Mod7 y) = lit (x + y)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of
    (Just y) -> Just (eval y)
    Nothing -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7