module Main where

import Control.Monad
import Lib

interactingFunction :: String -> String
interactingFunction s =
  case process s of
    Just f -> show f
    Nothing -> "Can't process '" ++ s ++ "'"

main = interact $ unlines . map interactingFunction . lines
