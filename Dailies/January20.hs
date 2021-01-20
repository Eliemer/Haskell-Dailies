-- Beginner, Intermediate, Expert in Haskell
import Data.List (partition)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

-- Beginner
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Intermediate
-- The stupid O(n^2) way
hasRep :: String -> Bool
hasRep [] = False
hasRep (x:xs) = 0 < (length . fst $ partition (==x) xs) || hasRep xs

-- The more efficient O(n log n) way
hasRep' :: String -> Bool
hasRep' xs = (length . S.fromList $ xs) /= (length xs)

-- An even more efficient O(n) way
hasRep'' :: String -> Bool
hasRep'' [] = False
hasRep'' (x:xs) = go xs $ M.singleton x x
  where go :: String -> M.HashMap Char Char -> Bool
        go [] map = False
        go (y:ys) map
            | (M.lookup y map) == Nothing = go ys $ M.insert y y map
            | otherwise                   = True

-- Expert
lcd :: Int -> Int -> Int
lcd x y = div (x * y) $ gcd x y
  where gcd a 0 = a
        gcd a b = b `seq` gcd b (a `mod` b) where

lcd' :: [Int] -> Int
lcd' xs = foldl1 lcd xs

main :: IO ()
main = do

  putStrLn "----\nBeginner\n----"
  traverse (print) $ myReverse <$> ["Hello", "Yes", "aBcDe"]

  putStrLn "\n----\nIntermediate\n----"
  traverse (print) $ hasRep <$> ["Hello", "Yes", "Alpha"]

  putStrLn "\n----\nExpert\n----"
  traverse (print) $ lcd' <$> [[1,2,3,4,5], [1,2,3,5,8], [1,2,3,6,7]]

  return ()