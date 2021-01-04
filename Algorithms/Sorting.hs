module Algorithms.Sorting
( insertionSort
, selectionSort
, quickSort
, quickSort'
) where

import Data.List (minimum, delete, partition)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert $ (insertionSort xs)
  where insert [] = [x]
        insert (y: ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs in x : selectionSort (delete x xs)

-- This one is infinite looping
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort xs = merge (mergeSort (fHalf xs)) (mergeSort (sHalf xs))
  where merge ys [] = ys
        merge [] zs = zs
        merge (y:ys) (z:zs) 
          | y <= z    = y : merge ys (z:zs)
          | otherwise = z : merge (y:ys) zs
        fHalf ws = let n = length ws in take (n `div` 2) ws
        sHalf ws = let n = length ws in drop (n `div` 2) ws

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