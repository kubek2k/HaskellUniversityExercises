module Lib
  ( process
  ) where

data OperatorType
  = Plus
  | Minus
  | Divide
  | Multiply
  deriving (Show)

data Token
  = Operator OperatorType
  | Operand Float
  deriving (Show)

instance Read Token where
  readsPrec _ "+" = [(Operator Plus, "")]
  readsPrec _ "-" = [(Operator Minus, "")]
  readsPrec _ "/" = [(Operator Divide, "")]
  readsPrec _ "*" = [(Operator Multiply, "")]
  readsPrec _ s = [(Operand (read s), "")]

tokenize = words

parse :: [String] -> [Token]
parse = map read

interpret :: [Token] -> Maybe Float
interpret tokens = resultingStack >>= return . head
  where
    foldingFunction Nothing _ = Nothing
    foldingFunction (Just stack) (Operand f) = Just (f : stack)
    foldingFunction (Just (x:y:tail)) (Operator o) =
      let operation =
            case o of
              Plus -> (+)
              Minus -> (-)
              Divide -> (/)
              Multiply -> (*)
      in Just (operation x y : tail)
    foldingFunction _ (Operator _) = Nothing
    resultingStack = foldl foldingFunction (Just []) tokens

process :: String -> Maybe Float
process = interpret . parse . tokenize
