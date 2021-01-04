-- Beginner
import Data.Char (ord, toLower)

-- Intermediate
import Data.Numbers.Primes

-- Expert
import Data.List (permutations)

-- Main
import Text.Printf

-- Beginner
textSum :: String -> Int
textSum xs = sum $ map (\x -> ord (toLower x) - 96) xs -- lambda version

textSum' :: String -> Int
textSum' xs = sum $ map ( (flip (-) 96) . ord . toLower ) xs -- pointfree version ??


-- Intermediate
primeSum :: Int -> Int
primeSum n = (+1) . sum $ [x | x <- [1..n], isPrime x]
-- 1 is not a prime, dont make me add this


-- Expert
similarPrime :: Int -> Int
similarPrime x = head . filter (/= x) $ findPrimes x
  where findPrimes = filter (isPrime) . fmap (read) . permutations. show


-- Main
main :: IO ()
main = do
  let a = "Hello"
      b = "Bye"
      c = "Sum"

  printf $ "Beginner: textSum\n"
  printf "%s -> %d\n" a $ textSum a
  printf "%s -> %d\n" b $ textSum b
  printf "%s -> %d\n" c $ textSum c

  let d = 16
      e = 19
      f = 5

  printf $ "Intermediate: primeSum\n"
  printf "%d -> %d\n" d $ primeSum d
  printf "%d -> %d\n" e $ primeSum e
  printf "%d -> %d\n" f $ primeSum f

  let g = 71
      h = 32
      i = 371

  printf $ "Expert: similarPrime\n"
  printf "%d -> %d\n" g $ similarPrime g
  printf "%d -> %d\n" h $ similarPrime h
  printf "%d -> %d\n" i $ similarPrime i