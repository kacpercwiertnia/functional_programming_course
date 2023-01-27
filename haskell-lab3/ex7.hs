import Data.Char
import Data.List

onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 == 1 = x : onlyOdd xs
 | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
 | fromEnum x >= 65 && fromEnum x <= 90 = x : onlyUpper xs
 | otherwise      = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise = filter' p xs

onlyEven'  = filter' even
onlyOdd'   = filter' odd
onlyUpper' = filter' (\x -> x >= 'A' && x <= 'Z')