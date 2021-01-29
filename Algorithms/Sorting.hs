module Algorithms.Sorting
( insertionSort
, selectionSort
, quickSort
, quickSort'
) where

import Data.List (minimum, delete, partition, splitAt)

-- bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort l = case sortPass l of
  d | d == l      -> l
    | otherwise   -> bubbleSort d
    where
      sortPass []       = []
      sortPass [x]      = [x]
      sortPass (x:y:xs)
        | x > y     = y : sortPass (x:xs)
        | otherwise = x : sortPass (y:xs) 
  

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert $ insertionSort xs
  where insert [] = [x]
        insert (y: ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs in x : selectionSort (delete x xs)

-- merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Bottom up version starts with making every element a list of length 1
-- and merges upwards, instead of splitting in halves first
-- ref : https://riptutorial.com/haskell/example/7552/merge-sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort xs = go [[x] | x <- xs]
  where 
    go :: Ord a => [[a]] -> [a]
    go [a] = a
    go ys  = go (pairs ys)

    -- left accumulate sorted lists with merge
    pairs :: Ord a => [[a]] -> [[a]]      
    pairs (a:b:as) = merge a b : pairs as
    pairs t = t

-- Damn thats impressive
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [a | a <- xs, a < x]
                    ++ [x] ++
                   quickSort [b | b <- xs, b >= x]

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = quickSort a ++ [x] ++ quickSort b
  where (a, b) = partition (< x) xs