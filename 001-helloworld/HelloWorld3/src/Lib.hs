module Lib
    ( recursiveAsk, foreverAsk ) 
where

import Control.Monad

askForName :: IO()
askForName = do
	putStrLn "Hello, whats your name?"
	name <- getLine
	putStrLn $ "Hello " ++ name

recursiveAsk :: IO ()
recursiveAsk = do
	askForName
	recursiveAsk

foreverAsk :: IO()
foreverAsk = forever askForName
