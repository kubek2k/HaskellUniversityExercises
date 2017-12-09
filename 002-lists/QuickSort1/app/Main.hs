module Main where

import Lib
import System.Environment

parseInts :: [String] -> [Int]
parseInts = map read

main :: IO ()
main = do
 	 args <- getArgs
	 print $ quickSort $ parseInts args 
