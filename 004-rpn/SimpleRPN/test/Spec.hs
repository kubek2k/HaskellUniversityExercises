import Data.List
import Lib
import Test.QuickCheck

genRandomFloat :: Gen String
genRandomFloat = (arbitrary :: Gen Float) >>= return . show

genRandomOperator = elements ["+", "-", "*", "/"]

genRandomStrings :: Int -> Gen [String]
genRandomStrings n =
  vectorOf n $ frequency [(2, genRandomFloat), (1, genRandomOperator)]

genRandomRPN :: Gen String
genRandomRPN =
  sized
    (\n -> do
       randomStrings <-
         vectorOf n $ frequency [(2, genRandomFloat), (1, genRandomOperator)]
       return $ intercalate " " randomStrings)

genProperRPN :: Gen String
genProperRPN =
  let genRandomFloatSingletonList = genRandomFloat >>= return . (: [])
      genRPNRecursively n =
        if (n == 0)
          then genRandomFloatSingletonList
          else do
            operand1 <-
              oneof [genRPNRecursively (n - 1), genRandomFloatSingletonList]
            operand2 <-
              oneof [genRPNRecursively (n - 1), genRandomFloatSingletonList]
            operator <- genRandomOperator >>= return . (: [])
            return $ concat [operand1, operand2, operator]
  in sized $ \n -> genRPNRecursively n >>= return . intercalate " "

prop_processDoesntFail :: Property
prop_processDoesntFail =
  forAll genRandomRPN $ \rpn ->
    case process rpn of
      (Just _) -> True
      Nothing -> True

prop_processSucceedsForProperRPN :: Property
prop_processSucceedsForProperRPN =
  forAll genProperRPN $ \rpn -> (process rpn /= Nothing)

main =
  quickCheck $
  conjoin [prop_processSucceedsForProperRPN, prop_processDoesntFail]
