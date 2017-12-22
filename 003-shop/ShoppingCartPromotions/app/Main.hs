module Main where

import Lib
import System.Environment (getArgs)

convertToFruit :: String -> Fruit
convertToFruit "a" = Apple
convertToFruit "o" = Orange

main :: IO ()
main =
  getArgs >>= return . map convertToFruit >>= return . checkoutPromotions >>=
  print
