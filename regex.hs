module Regex where

import Prelude
import Data.Char
import Debug.Trace

data TokenType = Literal | Operator | LeftParen | RightParen | EOF | InvalidToken deriving (Eq, Show)  
data OperatorType = Plus | Star | Dot | InvalidOperator deriving (Eq, Show)
type Operator = (OperatorType, Int)
type Token = (TokenType, String)
type Alphabet = String
type OperStack = [Token]

skipWhiteSpace :: String -> String
skipWhiteSpace "" = ""
skipWhiteSpace (x:xs) = if x == ' ' then skipWhiteSpace xs
                        else x:xs

readLiteral :: String -> Alphabet -> String
readLiteral "" a = ""
readLiteral (x:xs) a = if elem x a then x : (readLiteral xs a)
                  else ""

getOperator :: Char -> Operator
getOperator '+' = (Plus, 1)
getOperator '.' = (Dot, 2)
getOperator '*' = (Star, 3)
getOperator c = (InvalidOperator, -1)

readToken :: String -> Alphabet -> (Token, String)
readToken "" a = ((EOF, ""), "")
readToken s a = readInner (skipWhiteSpace s)
  where
    readInner :: String -> (Token, String)
    readInner (x:xs) =
        if elem x a then ((Literal, literal), drop (length literal) (x:xs))
        else if x == '(' then ((LeftParen, "("), xs)
        else if x == ')' then ((RightParen, ")"), xs)
        else if (fst (getOperator x)) /= InvalidOperator then ((Operator, [x]), xs)
        else ((InvalidToken, [x]), (x:xs))
        where
          literal = readLiteral (x:xs) a

readAllTokens :: String -> Alphabet -> [(Token, String)] -> [(Token, String)]
readAllTokens s a res =
  if tokenType == EOF then res
  else readAllTokens (snd token) a (res ++ [token])
  where
    token = readToken s a
    tokenType = fst (fst token)

popOperatorsUntilLP :: OperStack -> String -> (OperStack, String)
popOperatorsUntilLP [] res = error("Unbalanced parentheses")
popOperatorsUntilLP ((tokenType, tokenString):stack) res =
  if tokenType == LeftParen then (stack, res)
  else popOperatorsUntilLP stack (res ++ tokenString)

popOperatorsUntilEnd :: OperStack -> String -> String
popOperatorsUntilEnd [] res = res
popOperatorsUntilEnd ((tokenType, tokenString):stack) res =
  if tokenType == LeftParen then error("Unbalanced parentheses")
  else popOperatorsUntilEnd stack (res ++ tokenString)

popOperators :: OperStack -> String -> Token -> (OperStack, String)
popOperators [] res oper = ([oper], res)
popOperators (token:stack) res oper = 
  if (fst token) /= Operator then ((oper:token:stack), res)
  else if (snd operTop) <= (snd operStack) then popOperators stack (res ++ tokenStr) oper
  else ((oper:token:stack), res)
  where
    tokenStr = snd token
    operStack = getOperator (head tokenStr)
    operTop = getOperator (head $ snd oper)

-- 1) implicit .
-- 2) control operator arity
-- 3) create AST

parse :: String -> Alphabet -> String
parse s a = parseInner s a [] []
  where
    parseInner :: String -> Alphabet -> String -> OperStack -> String
    parseInner s a res stack =
      if tokenType == Literal then parseInner sRem a (res ++ tokenString) stack
      else if tokenType == LeftParen then parseInner sRem a res (token : stack)
      else if tokenType == RightParen then parseInner sRem a poppedRPRes poppedRPStack
      else if tokenType == Operator then parseInner sRem a operatorRes operatorStack       
      else if tokenType == EOF then popOperatorsUntilEnd stack res
      else error("Invalid token " ++ (show tokenString))
      where
        ((tokenType, tokenString), sRem) = readToken s a
        token = (tokenType, tokenString)
        (poppedRPStack, poppedRPRes) = popOperatorsUntilLP stack res
        (operatorStack, operatorRes) = popOperators stack res token