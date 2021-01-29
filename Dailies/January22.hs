{-# LANGUAGE MultiWayIf #-}

-- Beginner, Intermediate, and Expert in Haskell
import Data.List (subsequences, isPrefixOf)
import Data.Char (isDigit)

-- Beginner
-- Reaaally bad big-O, something like O(2^n) because of subsequences
bomb :: String -> String
bomb xs
  | "bomb" `elem` (subsequences xs)  = "Duck!!!"
  | otherwise = "There is no bomb, relax."

-- O(n + len("bomb"))
bomb' :: String -> String
bomb' []       = "There is no bomb"
bomb' l@(x:xs)
  | "bomb" `isPrefixOf` l = "Duck!!!" 
  | otherwise             = bomb' xs

-- Intermediate
parseInt :: String -> Maybe Integer
parseInt [] = Nothing
parseInt z  = if (and $ isDigit <$> z) 
                then Just $ read z
                else Nothing

addStrNums :: (String, String) -> String
addStrNums (x,y) = show $ go (parseInt x) (parseInt y)
  where go (Just a) (Just b) = a + b
        go _ _               = -1
        


addStrNums' :: (String, String) -> String
addStrNums' (x, y) = case ((parseInt x), (parseInt y)) of
                    ((Just a), (Just b)) -> show $ a + b
                    otherwise            -> "-1"


-- Main
main :: IO ()
main = do

  putStrLn "----\nBeginner\n----"
  traverse (print) $ bomb' <$> ["There is a bomb.", "Hey, did you think there is a bomb?", "This goes boom!!!"]

  putStrLn "\n----\nIntermediate\n----"
  traverse (print) $ addStrNums <$> [("4", "5"), ("abc", "3"), ("1", ""), ("1874682736267235927359283579235789257", "32652983572985729")]

  return ()