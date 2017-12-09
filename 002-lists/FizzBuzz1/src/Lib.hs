module Lib
    ( fizzBuzz
    ) where

fizzBuzz :: Int -> [String]
fizzBuzz n = take n $ map fizzElem [i | i <- [0..]] 

fizzElem :: Int -> String
fizzElem i  
	 | i `mod` 15 == 0 = "Fizz Buzz"
         | i `mod` 3 == 0 = "Fizz"
	 | i `mod` 5 == 0 = "Buzz"
	 | otherwise = show i

