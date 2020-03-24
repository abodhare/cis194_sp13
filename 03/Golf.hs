module Golf where

skips :: [a] -> [[a]]
skips = loopOver 1
    where loopOver n x
            | n > length x = []
            | otherwise = takeEveryN n x : loopOver (n + 1) x

takeEveryN :: Int -> [a] -> [a]
takeEveryN _ [] = []
takeEveryN n x = counter n (drop (n - 1) x)
    where counter _ [] = []
          counter m list@(y:_) = y : counter m (drop m list)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x < y && y > z = y : localMaxima (y:z:zs)
    | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

numList :: [Integer] -> [Int]
numList = summarize 0
    where summarize n x
            | n >= 0 && n < 10 = length (filter (== n) x) : summarize (n + 1) x
            | otherwise = []

extractString :: [Int] -> String
extractString = map (\x -> if x > 0 then '*' else ' ')

extractStrings :: [Int] -> [String]
extractStrings [] = []
extractStrings x 
    | length (filter (== 0) x) /= 10 = extractString x : extractStrings (map (\y -> if y > 0 then y - 1 else y) x)
    | otherwise = []

histogram :: [Integer] -> String
histogram x = (unlines . reverse . extractStrings . numList) x ++ "==========\n0123456789"