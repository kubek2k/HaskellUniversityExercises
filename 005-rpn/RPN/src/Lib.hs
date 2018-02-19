module Lib
  ( process
  ) where

import Data.List
import Data.Maybe
import Control.Applicative
import Text.Read

data OperatorType
  = Plus
  | Minus
  | Divide
  | Multiply
  deriving (Show)

data Token
  = Operator OperatorType
  | VariableRef Int
  | Operand Float
  deriving (Show)

readVariableRef :: String -> Maybe Token
readVariableRef s = do
  stripped <- stripPrefix "res" s
  readMaybe stripped

instance Read Token where
  readsPrec _ "+" = [(Operator Plus, "")]
  readsPrec _ "-" = [(Operator Minus, "")]
  readsPrec _ "/" = [(Operator Divide, "")]
  readsPrec _ "*" = [(Operator Multiply, "")]
  readsPrec _ s = [(fromJust((readVariableRef s) <|> (readMaybe s)), "")]

tokenize :: String -> [String]
tokenize = words

parse :: [String] -> Maybe [Token]
parse = sequence . (map readMaybe)

interpret :: [Token] -> Maybe Float
interpret tokens = resultingStack >>= return . head
  where
    foldingFunction Nothing _ = Nothing
    foldingFunction (Just stack) (Operand f) = Just (f : stack)
    foldingFunction (Just stack) (VariableRef _) = Just (1.0 : stack)
    foldingFunction (Just (x:y:rest)) (Operator o) =
      let operation =
            case o of
              Plus -> (+)
              Minus -> (-)
              Divide -> (/)
              Multiply -> (*)
      in Just (operation x y : rest)
    foldingFunction _ (Operator _) = Nothing
    resultingStack = foldl foldingFunction (Just []) tokens

process :: String -> Maybe Float
process s = return (tokenize s) >>= parse >>= interpret
