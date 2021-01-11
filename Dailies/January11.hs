import Data.Char

-- Beginner
carTimer :: Int -> Int
carTimer n = sum $ digitToInt <$> digits
  where h = show (n `quot` 60)
        m = show (n `rem` 60)
        digits = h ++ m

-- Intermediate
incrementString :: String -> String
incrementString xs = letters ++ paddedNumber
  where (letters, number) = span isLetter xs
        parsedNumber      = if length number > 0 then read number else 0
        paddedNumber      = lpad (length number) (show $ parsedNumber + 1)
        lpad m ys         = replicate (m - length zs) '0' ++ ys
            where zs      = take m ys

-- Expert


main :: IO ()
main = do
  traverse (print) $ carTimer <$> [240, 808]
  traverse (putStrLn) $ incrementString <$> ["foo", "foobar0009", "foo099"]

  return ()