module Lib
  ( checkout
  , checkoutPromotions
  , Fruit(..)
  ) where

import Data.Map (Map, foldlWithKey, fromListWith, mapWithKey)

data Fruit
  = Apple
  | Orange
  deriving (Eq, Ord, Show)

price :: Fruit -> Integer
price Apple = 60
price Orange = 25

countFruits :: [Fruit] -> Map Fruit Integer
countFruits fruits =
  let mapped = map (\v -> (v, 1)) fruits
  in fromListWith (+) mapped

sumUp :: Map Fruit Integer -> Integer
sumUp = foldlWithKey (\acc f n -> acc + n * (price f)) 0

promotion :: Fruit -> Integer -> Integer
promotion Apple c = (c `div` 2) + (c `rem` 2)
promotion Orange c = 2 * (c `div` 3) + (c `rem` 3)

applyPromotions :: Map Fruit Integer -> Map Fruit Integer
applyPromotions = mapWithKey promotion

checkoutPromotions :: [Fruit] -> Integer
checkoutPromotions = sumUp . applyPromotions . countFruits

checkout :: [Fruit] -> Integer
checkout = sumUp . countFruits
