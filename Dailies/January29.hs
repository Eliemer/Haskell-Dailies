-- Beginner, Itermediate, Expert in Haskell
import Foreign.Marshal.Utils (fromBool)

-- Beginner
countTrue :: [Bool] -> Integer
countTrue = sum . fmap fromBool 

-- Intermediate
sumDigProd :: [Integer] -> Integer
sumDigProd = go . show . sum
  where 
    go str
      | length str > 1 = go . show . product . fmap (read . (:[])) $ str
      | length str > 0 = read str
      | otherwise      = 0

-- Expert
zeroIndices :: Integer -> [Integer] -> [Integer]
zeroIndices _ _ = [0]

substitutions :: a -> [a] -> [[a]]
substitutions x y = go 0 x y
  where go :: Int -> a -> [a] -> [[a]]
        go n x [] = mempty
        go n x y 
          | n < length y = [take n y ++ [x] ++ drop (n+1) y] ++ go (n+1) x y
          | otherwise    = mempty

substitutions' :: a -> [a] -> [[a]]
substitutions' x ys = [[if i == j then x else ys !! (j-1) | j <- [1..(length ys)]] | i <- [1..(length ys)]]

-- Main
main :: IO ()
main = do

  putStrLn "\n----\nBeginner\n----\n"
  traverse (print) $ countTrue <$> [ [True, False, False, True, False]
                                   , [False, False, False, False]
                                   , []
                                   ]

  putStrLn "\n----\nIntermediate\n----\n"
  traverse (print) $ sumDigProd <$> [ [16, 28]
                                    , [0]
                                    , [1..6]
                                    ]
  
  -- putStrLn "\n----\nExpert\n----\n"
  -- traverse (print) $ zeroIndices <$> [ [1, 0, 1, 1, 0, 0, 0, 1]
  --                                    , [1, 0, 0, 0, 0, 1]
  --                                    ]
  
  return ()