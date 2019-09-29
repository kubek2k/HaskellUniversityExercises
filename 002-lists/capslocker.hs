import Control.Monad
import Data.Char

main :: IO ()
main =
  forever $ do
    l <- getLine
    putStrLn $ map toUpper l
