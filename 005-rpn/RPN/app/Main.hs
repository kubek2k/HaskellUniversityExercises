module Main where

import Lib

interactingFunction :: String -> String
interactingFunction s =
  case process s of
    Just f -> show f
    Nothing -> "Can't process '" ++ s ++ "'"

main :: IO ()
main = interact $ unlines . map interactingFunction . lines
