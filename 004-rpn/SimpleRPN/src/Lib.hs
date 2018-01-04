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
interpret tokens = interpretInternal tokens []
  where
    interpretInternal (Operand f:rest) stack =
      interpretInternal rest (f : stack)
    interpretInternal (Operator o:rest) (stackHead:stackTailHead:stackTailTail) =
      let operation =
            case o of
              Plus -> (+)
              Minus -> (-)
              Divide -> (/)
              Multiply -> (*)
      in interpretInternal
           rest
           (operation stackTailHead stackHead : stackTailTail)
    interpretInternal (Operator _:_) _ = Nothing
    interpretInternal [] [stackHead] = Just stackHead
    interpretInternal [] _ = Nothing

process :: String -> Maybe Float
process = interpret . parse . tokenize
