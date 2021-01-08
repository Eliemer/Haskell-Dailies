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
myLength = go 0
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
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (xs >>= flatten)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y      = compress (y:xs)
  | otherwise   = x : compress (y:xs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = myReverse $ go [[x]] xs
  where go acc [] = acc
        go acc@(g@(w:ws):gs) (y:ys)
          | y == w      = go ((y:g):gs) ys
          | otherwise   = go ([y]:acc) ys

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = (\a -> (myLength a, head a)) <$> pack xs

-- Problem 11
data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = go . encode
  where go :: [(Int, a)] -> [Encoded a]
        go [] = []
        go ((n, x): xs)
            | n == 1    = Single x : go xs
            | otherwise = Multiple n x : go xs

-- Problem 12
decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((Single x): xs) = x: decodeModified xs
decodeModified ((Multiple n x): xs) = [x | n0 <- [1..n]] ++ decodeModified xs

-- Problem 13
-- encodeDirect :: Eq a => [a] -> [Encoded a]

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x: dupli xs

-- Problem 15
repli :: Int -> [a] -> [a]
repli _ [] = []
repli n (x:xs) = [x | _<-[1..n]] ++ repli n xs

-- repli' :: Int -> [a] -> [a]
-- repli' n = fmap (replicate n)

-- Problem 16
-- the stupid way
dropEvery :: Int -> [a] -> [a]
dropEvery n xs = go 0 xs
  where go m ys
    |
    |