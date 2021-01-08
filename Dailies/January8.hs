import Data.List (minimumBy)

-- Beginner 
drawBox :: Int -> Int -> [String]
drawBox n m = replicate n $ replicate m '|'

-- Intermediate
infix 5 &>>
(&>>) :: Int -> Int -> Int
a &>> b = (a) `div` (2 ^ b)

-- Expert
nearestChapter :: [(String, Int)] -> Int -> String
nearestChapter map n = fst $ minimumBy (ordering) map
  where ordering (_, a) (_, b) 
            | c == d = compare a b
            | otherwise = compare c d
            where c = abs $ a-n
                  d = abs $ b-n

main :: IO ()
main = do

  -- Beginner
  putStrLn $ "\ndrawBox (2, 4)"
  traverse (putStrLn) $ drawBox 2 4

  putStrLn $ "\ndrawBox (3, 5)"
  traverse (putStrLn) $ drawBox 3 5

  putStrLn $ "\ndrawBox (1, 3)"
  traverse (putStrLn) $ drawBox 1 3

  putStrLn $ "\ndrawBox (3, 2)"
  traverse (putStrLn) $ drawBox 3 2

  -- Intermediate
  putStrLn "-----"
  putStrLn $ "\n80 >> 3"
  print $ 80 &>> 3

  putStrLn $ "\n-24 >> 2"
  print $ (-24) &>> 2

  putStrLn $ "\n-5 >> 1"
  print $ (-5) &>> 1

  -- Expert
  putStrLn "-----"
  putStrLn $ nearestChapter [ ("Chapter 1", 1), ("Chapter 2", 15), ("Chapter 3", 37) ] 10

  putStrLn $ nearestChapter [ ("New Beginnings", 1), ("Strange Developments", 62), ("The End?", 194), ("The True Ending", 460)] 200

  putStrLn $ nearestChapter [ ("Chapter 1a", 1), ("Chapter 1b", 5) ] 3

  return ()