import System.Random.Shuffle
import Criterion.Main
import Algorithms.Sorting

main :: IO ()
main = do
  y <- shuffleM [(1::Int)..(100000::Int)]
  defaultMain
    [
      bgroup "quickSort"
      [ bench "10" $ nf quickSort y
      , bench "100" $ nf quickSort y
      ,   bench "1000" $ nf quickSort y
      ,   bench "10000" $ nf quickSort y
      ,   bench "100000" $ nf quickSort y
      ]
    ]
