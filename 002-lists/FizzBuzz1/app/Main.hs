module Main where
import System.Environment
import System.Exit
import Lib

main :: IO ()
main = getArgs >>= return . read . head >>= (\n -> return $ fizzBuzz n) >>= print
