import Test.QuickCheck
import Lib

countOccurrences :: String -> [String] -> Int
countOccurrences s l = length $ filter ((==) s) l

prop_aroundOneThirdAreFizz :: Int -> Bool
prop_aroundOneThirdAreFizz n = (countOccurrences "Fizz" (fizzBuzz n)) == ((quot (n - 1) 3) - (quot (n - 1) 15))  || n <= 0

prop_aroundOneFifthAreBuzz :: Int -> Bool
prop_aroundOneFifthAreBuzz n = (countOccurrences "Buzz" (fizzBuzz n)) == ((quot (n - 1) 5) - (quot (n - 1) 15)) || n <= 0

prop_aroundOneFifteenthAreFizzBuzz :: Int -> Bool
prop_aroundOneFifteenthAreFizzBuzz n = (countOccurrences "Fizz Buzz" (fizzBuzz n)) == ((quot (n - 1) 15) + 1) || n <= 0

main :: IO ()
main = quickCheck $ conjoin [prop_aroundOneThirdAreFizz, prop_aroundOneFifthAreBuzz, prop_aroundOneFifteenthAreFizzBuzz]
