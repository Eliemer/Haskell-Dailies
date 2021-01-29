-- Beginner, Intermediate, Expert in Haskell
import Data.Char (ord)

-- Beginner
isUpper :: Char -> Bool
isUpper x = let x' = ord x in x' >= 65 && x' <= 90

-- Intermediate
myMax :: Ord a => [a] -> a
myMax = foldl1 (\a b -> if a > b then a else b)

-- Expert 


-- Main
main :: IO ()
main = do

  putStrLn "----\nBeginner\n----"
  traverse (print) $ isUpper <$> "aA7"

  putStrLn "\n----\nIntermediate\n----"
  traverse (print) $ myMax <$> [[1..5], [10,9..6], [5,9,2,7,4], [1..9]]

  -- putStrLn "\n----\nExpert\n----"

  return ()