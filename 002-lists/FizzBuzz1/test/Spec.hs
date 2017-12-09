import Test.QuickCheck
import Lib

countOccurrences :: String -> [String] -> Int
countOccurrences s l = length $ filter ((==) s) l

prop_aroundOneThirdAreFizz :: NonNegative Int -> Property
prop_aroundOneThirdAreFizz (NonNegative n) = n > 0 ==> (countOccurrences "Fizz" (fizzBuzz n)) == ((quot (n - 1) 3) - (quot (n - 1) 15)) 

prop_aroundOneFifthAreBuzz :: NonNegative Int -> Property
prop_aroundOneFifthAreBuzz (NonNegative n) = n > 0 ==> (countOccurrences "Buzz" (fizzBuzz n)) == ((quot (n - 1) 5) - (quot (n - 1) 15))

prop_aroundOneFifteenthAreFizzBuzz :: NonNegative Int -> Property
prop_aroundOneFifteenthAreFizzBuzz (NonNegative n) = n > 0 ==> (countOccurrences "Fizz Buzz" (fizzBuzz n)) == ((quot (n - 1) 15) + 1) 

main :: IO ()
main = quickCheck $ conjoin [prop_aroundOneThirdAreFizz, prop_aroundOneFifthAreBuzz, prop_aroundOneFifteenthAreFizzBuzz]
