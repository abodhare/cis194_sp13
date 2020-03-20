toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)
toDigitsRev _ = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEO (reverse l))
    where doubleEO [] = []
          doubleEO (x:[]) = [x]
          doubleEO (x:y:xs) = x : (2*y) : (doubleEO xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (foldl (+) 0 (toDigitsRev x)) + sumDigits xs

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ ((a, b) : hanoi (n-1) c b a)