-- Beginner, Intermediate, Expert in Haskell
import Data.List (sort, splitAt)
import Data.Char (toLower, ord)

-- Beginner
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = reverse $ go [[x]] xs
  where go acc [] = acc
        go acc@(g@(w:ws):gs) (y:ys)
          | y == w      = go ((y:g):gs) ys
          | otherwise   = go ([y]:acc) ys

occur :: String -> [(Char, Int)]
occur l = (pack . sort $ toLower <$> l) >>= (\a -> [(a !! 0, length a)])

-- Intermediate
paths :: Integer -> Integer
paths n = product [1..n]

-- Expert
balanced :: String -> Bool
balanced xs
  | length xs `mod` 2 == 0  = sum a' == sum b'
  | otherwise               = sum a' == sum (tail b')
  where (a,b) = splitAt (length xs `div` 2) (toLower <$> xs)
        a'    = a >>= (\x -> [ord x - 97])
        b'    = b >>= (\x -> [ord x - 97])

-- Main
main :: IO ()
main = do

  putStrLn "----\nBeginner\n----"
  traverse (print) $ occur <$> ["Hello World", "Alphabet"]

  putStrLn "\n----\nIntermediate\n----"
  traverse (print) $ paths <$> [4, 1, 9]

  putStrLn "\n----\nExpert\n----"
  traverse (print) $ balanced <$> ["zips", "brake", "abba"]

  return ()