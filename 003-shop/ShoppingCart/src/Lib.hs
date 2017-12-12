module Lib
    ( checkout, Fruit
    ) where

data Fruit = Apple | Orange
price :: Fruit -> Integer
price Apple = 60
price Orange = 25 

checkout :: [Fruit] -> Integer
checkout fruits = sum $ map price fruits
