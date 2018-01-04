module Main where

import Lib

main = do
  line <- getLine
  print (process line)
