module Main where

import Lib
import System.Environment

parseInts :: [String] -> [Int]
parseInts = map read

mapAndReturn :: Monad m => m a -> (a -> b) -> m b
mapAndReturn ma f = fmap f ma

infixl 4 >>>
(>>>) = mapAndReturn :: (IO a -> (a -> b) -> IO b)

main :: IO ()
main = getArgs >>> parseInts >>= print
