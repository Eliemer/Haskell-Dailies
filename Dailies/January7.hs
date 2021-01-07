import Data.Char (ord, chr, isSpace, isUpper, toLower)
import Data.List (sortBy, intercalate)
import Data.List.Split (wordsBy)

-- Beginner
averageChar :: String -> Char
averageChar xs = chr . (\a -> a `div` length xs) . sum $ ord <$> xs

-- Intermediate
spaceString :: String -> String
spaceString (x:xs) = x : (concat $ replace <$> xs)
  where replace = (\c -> if isUpper c then (' ' : toLower c : []) else c : [])

-- Expert
sortString :: String -> String
sortString xs = intercalate " " $ sortBy ordering splitString
  where splitString  = wordsBy isSpace xs
        ordering a b = compare (averageChar a) (averageChar b)

main :: IO ()
main = do
  traverse (print) $ averageChar <$> ["Hello", "Yes", "aaaa"]

  traverse (print) $ spaceString <$> ["HelloWorld!", "HowAreYou?", "Example", "Simple,Right?"]

  traverse (print) $ sortString <$> ["Hello Yes aaaa", "My string Example"]

  return ()