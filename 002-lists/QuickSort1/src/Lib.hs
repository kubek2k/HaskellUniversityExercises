module Lib
  ( quickSort
  , quickSortInt
  ) where

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerAndEqualSorted = quickSort [v | v <- xs, v <= x]
      largerSorted = quickSort [v | v <- xs, v > x]
  in smallerAndEqualSorted ++ [x] ++ largerSorted

quickSortInt :: [Int] -> [Int]
quickSortInt = quickSort
