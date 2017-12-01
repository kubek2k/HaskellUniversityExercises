module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
	putStrLn "Hello, what is your name?"
	name <- getLine
	putStrLn $ "Nice to meet you " ++ name
