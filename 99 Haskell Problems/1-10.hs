-- Problem 1
myLast :: [a] -> a
myLast [x]  = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs n = go 1 xs
  where 
    go acc [] = error "index too large"
    go acc (y:ys)
      | acc == n  = y
      | otherwise = go (acc + 1) ys

-- Problem 4
myLength :: [a] -> Int
myLength = go 1
  where go acc []     = acc
        go acc (x:xs) = go (acc + 1) xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalidrome :: Eq a => [a] -> Bool
isPalidrome xs = xs == myReverse xs 

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) : flatten xs