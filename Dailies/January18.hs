-- Beginner, Intermediate, and Expert in Haskell
import Data.List
-- import Data.List.Split
import qualified Data.Text as T
import Data.Char (ord)

-- Beginner
-- derivate :: (Fractional a, Integral a) => a -> a -> a
derivative :: Int -> Int -> Int
derivative b x = (*b) $ x ^ (b-1)

-- Intermediate
isVirus :: String -> Bool
isVirus xs
  | xs == "notvirus.exe" = False
  | xs == "antivirus.exe" = False
  | T.pack "virus" `T.isInfixOf` T.pack xs = True
  | T.pack "malware" `T.isInfixOf` T.pack xs = True
  | otherwise = False

removeVirus :: String -> String
removeVirus xs = "PC Files: " ++ (intercalate ", " clean)
  where list = T.splitOn (T.pack ", ") $ T.pack (drop 10 xs)
        clean = filter (not . isVirus) $ T.unpack <$> list

-- Expert
distanceToNearestVowel :: String -> [Int]
distanceToNearestVowel xs = minDist <$> xs
  where minDist x = minimum $ fmap (\a -> abs $ a - ord(x)) (ord <$> "aeiou")

main :: IO ()
main = do

  putStrLn "----\nBeginner\n----"
  traverse (print) $ derivative <$> [1, 3, 4] <*> [4, (-2), (-3)]
  putStrLn "----\n"

  putStrLn "----\nIntermediate\n----"
  traverse (print) $ removeVirus <$> ["PC Files: spotifysetup.exe, virus.exe, dog.jpg", "PC Files: antivirus.exe, cat.pdf, lethalmalware.exe, dangerousvirus.exe "]
  putStrLn "----\n"

  putStrLn "----\nExpert\n----"
  traverse (print) $ distanceToNearestVowel <$> ["aaaaa", "babbb", "abcdabcd"]
  putStrLn "----\n"

  
  return ()