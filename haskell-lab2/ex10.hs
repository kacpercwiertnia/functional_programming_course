fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Div :: (Eq a, Integral a) => [a] -> Bool
fst2Div (x : y : _) | y `mod` x == 0 = True
fst2Div _                    = False

fstDivthr :: (Eq a, Integral a) => [a] -> Bool
fstDivthr (x : y : z: _) | z `mod` x == 0 = True
fstDivthr _                    = False