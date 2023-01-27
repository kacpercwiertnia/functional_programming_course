pitTria100 = [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n = n `elem` (take n primes)

primesIn1000 = length [x | x <- [2..1000], isPrime x]
