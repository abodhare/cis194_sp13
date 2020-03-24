{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Buffer
import Sized
import Scrabble

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

val :: Monoid a => JoinList m a -> a
val Empty = mempty
val (Single _ a) = a
val (Append _ l r) = val l <> val r

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ n (Single m a)
    | getSize (size m) == (n + 1) = Just a
    | otherwise = Nothing
indexJ n (Append m x y)
    | (n + 1) > getSize (size m) = Nothing
    | (n + 1) <= getSize (size . tag $ x) = indexJ n x
    | otherwise = indexJ (n - getSize (size . tag $ x)) y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0 = x
dropJ n x | n >= (getSize . size . tag $ x) = Empty
dropJ n (Append m x y)
    | n < sizeX = dropJ n x +++ y
    | n == sizeX = y
    | otherwise = dropJ (n - sizeX) y
    where sizeX = getSize . size . tag $ x

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n x | n <= 0 = Empty
takeJ n x | n >= (getSize . size . tag $ x) = x
takeJ n (Append m x y)
    | n < sizeX = takeJ n x
    | n == sizeX = x
    | otherwise = x +++ takeJ (n - sizeX) y
    where sizeX = getSize . size . tag $ x

scoreLine :: String -> JoinList (Score, Size) String
scoreLine x = Single (scoreString x, Size 1) x

instance Buffer (JoinList (Score, Size) String) where
    toString = val
    fromString = foldr ((+++) . scoreLine) Empty . lines
    line = indexJ
    replaceLine n str x
        | n < 0 || n >= (getSize . size . tag $ x) = x
        | otherwise = takeJ n x +++ scoreLine str +++ dropJ (n + 1) x
    numLines = getSize . size . tag
    value = getScore . fst . tag
