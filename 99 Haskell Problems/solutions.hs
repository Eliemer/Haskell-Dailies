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
dropEvery n xs = go 1 xs
  where go _ [] = []
        go m (y:ys)
          | m `mod` n == 0  = go 1 ys
          | otherwise       = y : go (m+1) ys

-- Problem 17
split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

split' :: Int -> [a] -> ([a], [a])
split' _ []       = ([], [])
split' n l@(x:xs)
    | n > 0       = (x : ys, zs)
    | otherwise   = ([], l)
    where (ys,zs) = split (n - 1) xs 

-- Problem 18
slice :: Int -> Int -> [a] -> [a]
slice s e xs = drop (s-1) $ take e xs

-- Problem 19
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n l@(x:xs)
  | n > 0       = rotate (n-1) $ xs ++ [x]
  | n < 0       = rotate (n+1) $ (last l : init l)
  | otherwise   = l

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error ""
removeAt n xs = if n > 1 then go n [] xs else error ""
  where go m ys [] = error ""
        go m ys (z:zs)
              | m > 1     = go (m-1) (ys ++ [z]) zs 
              | otherwise = (z, ys ++ zs)