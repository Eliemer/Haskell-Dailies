-- Beginner, Itermediate, Expert in Haskell

-- Beginner

-- Intermediate

-- Expert

-- Main
main :: IO ()
main = do

  putStrLn "\n----\nBeginner\n----\n"
  traverse (print) $ <Beginner> <$> [ARGS]

  putStrLn "\n----\nIntermediate\n----\n"
  traverse (print) $ <Intermediate> <$> [ARGS]
  
  putStrLn "\n----\nExpert\n----\n"
  traverse (print) $ <Expert> <$> [ARGS]
  
  return ()