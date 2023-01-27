import Data.List

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl (&&) True (zipWith (==) xs (sort xs))


everySecond :: [t] -> [t]
everySecond xs = map (\(x,y) -> x)  (filter (\(x,y) -> odd y) (zip xs [1..(length xs)]))
