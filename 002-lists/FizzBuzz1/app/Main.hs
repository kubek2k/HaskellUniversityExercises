module Main where
import System.Environment
import System.Exit
import Lib

parseArg :: [String] -> IO Int
parseArg [] = exitWith (ExitFailure 1)
parseArg (x:xs) = return $ read x

main :: IO ()
main = getArgs >>= parseArg >>= (\n -> return $ fizzBuzz n) >>= print
