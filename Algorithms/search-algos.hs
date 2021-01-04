linearSearch :: (Eq a) => a -> [a] -> Integer
linearSearch n [] = -1
linearSearch n (x : xs) = recLinearSearch 0 n (x : xs)

recLinearSearch :: (Eq a) => Integer -> a -> [a] -> Integer
recLinearSearch idx n [] = -1
recLinearSearch idx n (x : xs)
        | n == x = idx
        | otherwise = recLinearSearch (idx + 1) n xs

