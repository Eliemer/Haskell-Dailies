-- Beginner, Intermediate, Expert in Haskell
import Data.List

-- Beginner
countTrue :: [Bool] -> Int
countTrue = foldl (\acc el -> if el then acc + 1 else acc + 0) 0

-- Intermediate
removeLetters :: String -> String -> String
removeLetters [] ys = ys
removeLetters _ []  = []
removeLetters (x:xs) (y:ys) = removeLetters xs $ go x (y:ys)
  where go _ [] = []
        go a (b:bs)
          | a == b      = bs
          | otherwise   = b : go a bs

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere b [] = [[b]]
insertEverywhere b l@(x:xs) = (b:l) : map (x:) (insertEverywhere b xs)

maxmin :: Int -> (String, String)
maxmin xs = do
  
  (maximum oneSwapPerms, minimum oneSwapPerms)
