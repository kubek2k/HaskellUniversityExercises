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

prop_processDoesntFail :: Property
prop_processDoesntFail =
  forAll genRandomRPN $ \rpn ->
    case process rpn of
      (Just _) -> True
      Nothing -> True

genProperRPN :: Gen String
genProperRPN =
  let genRandomFloatSingletonList = genRandomFloat >>= return . (: [])
      genRPNRecursively n =
        if (n == 0)
          then genRandomFloatSingletonList
          else fmap concat $
               sequence
                 [ oneof
                     [genRPNRecursively (n - 1), genRandomFloatSingletonList]
                 , oneof
                     [genRPNRecursively (n - 1), genRandomFloatSingletonList]
                 , genRandomOperator >>= return . (: [])
                 ]
  in sized $ \n -> genRPNRecursively n >>= return . intercalate " "

prop_processSucceedsForProperRPN :: Property
prop_processSucceedsForProperRPN =
  forAll genProperRPN $ \rpn -> (process rpn /= Nothing)

main =
  quickCheck $
  conjoin [prop_processSucceedsForProperRPN, prop_processDoesntFail]
