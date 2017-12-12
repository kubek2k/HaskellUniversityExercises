import Test.QuickCheck
import Lib

genFruits :: Gen [Fruit]
genFruits = listOf $ elements [Apple, Orange]

prop_IsCheaperAfterPromotion fruits = (length fruits) > 5 ==> (checkout fruits) > (checkoutPromotions fruits) where types = fruits :: [Fruit]
 
main :: IO ()
main = quickCheck $ forAll genFruits prop_IsCheaperAfterPromotion
