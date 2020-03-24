{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = iter 0 1
    where iter a b = a : iter b (a + b)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a b) c = Cons a (interleaveStreams c b)

ruler :: Stream Integer
ruler = interleave 0
    where interleave n = interleaveStreams (streamRepeat n) (interleave (n + 1))

x :: Num a => Stream a
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num a => Num (Stream a) where
    fromInteger n = Cons (fromInteger n) (streamRepeat 0)
    negate = streamMap negate
    (+) (Cons a0 a) (Cons b0 b) = Cons (a0 + b0) (a + b)
    (*) (Cons a0 a) p@(Cons b0 b) = Cons (a0 * b0) (streamMap (* a0) b + a * p)

instance Fractional a => Fractional (Stream a) where
    (/) a@(Cons a0 as) b@(Cons b0 bs) = Cons (a0 / b0) (streamMap (*(1/b0)) (as - ((a / b) * bs)))

instance Fractional Integer where
    (/) = div

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer
instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22) (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case Matrix 1 1 1 0 ^ n of
    (Matrix _ a _ _) -> a