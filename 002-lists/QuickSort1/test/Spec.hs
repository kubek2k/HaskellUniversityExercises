import Test.QuickCheck
import Lib 
import Data.Map.Strict (Map, fromListWith)
import Data.List

prop_retainsElements :: Ord a => [a] -> Bool
prop_retainsElements l = hasAllElements (quickSort l) l
                          where
                            hasAllElements original result = null (original \\ result) && null (result \\ original)

prop_sorted :: Ord a => [a] -> Bool
prop_sorted l = let 
               sorted = quickSort l
               zipped = zip sorted (drop 1 sorted)
               inOrder = uncurry (<=)
             in
               all inOrder zipped

main :: IO ()
main = quickCheck $ conjoin [prop_sorted :: [Integer] -> Bool, prop_retainsElements]
