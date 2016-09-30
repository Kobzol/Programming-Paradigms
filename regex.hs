module Regex where

import Prelude
import Data.Char
import NFA
import Debug.Trace

data TokenType = Literal | Operator | LeftParen | RightParen | EOF | InvalidToken deriving (Eq, Show)  
data OperatorType = Plus | Star | InvalidOperator deriving (Eq, Show)
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
getOperator '*' = (Star, 2)
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

popOperatorsUntilLP :: OperStack -> [Token] -> (OperStack, [Token])
popOperatorsUntilLP [] res = error("Unbalanced parentheses")
popOperatorsUntilLP ((tokenType, tokenString):stack) res =
  if tokenType == LeftParen then (stack, res)
  else popOperatorsUntilLP stack (res ++ [(tokenType, tokenString)])

popOperatorsUntilEnd :: OperStack -> [Token] -> [Token]
popOperatorsUntilEnd [] res = res
popOperatorsUntilEnd ((tokenType, tokenString):stack) res =
  if tokenType == LeftParen then error("Unbalanced parentheses")
  else popOperatorsUntilEnd stack (res ++ [(tokenType, tokenString)])

popOperators :: OperStack -> [Token] -> Token -> (OperStack, [Token])
popOperators [] res oper = ([oper], res)
popOperators (token:stack) res oper = 
  if (fst token) /= Operator then ((oper:token:stack), res)
  else if (snd operTop) <= (snd operStack) then popOperators stack (res ++ [token]) oper
  else ((oper:token:stack), res)
  where
    tokenStr = snd token
    operStack = getOperator (head tokenStr)
    operTop = getOperator (head $ snd oper)

parse :: String -> Alphabet -> [Token]
parse s a = parseInner s a [] []
  where
    parseInner :: String -> Alphabet -> [Token] -> OperStack -> [Token]
    parseInner s a res stack =
      if tokenType == Literal then parseInner sRem a (res ++ [token]) stack
      else if tokenType == LeftParen then parseInner sRem a res (token : stack)
      else if tokenType == RightParen then parseInner sRem a poppedRPRes poppedRPStack
      else if tokenType == Operator then parseInner sRem a operatorRes operatorStack       
      else if tokenType == EOF then popOperatorsUntilEnd stack res
      else error("Invalid token " ++ (show tokenString))
      where
        ((tokenType, tokenString), sRem) = readToken s a
        token = (tokenType, tokenString)
        (poppedRPStack, poppedRPRes) = popOperatorsUntilLP stack res
        (operatorType, _) = getOperator (head (tokenString))
        (operatorStack, operatorRes) = if operatorType == Star then (stack, res ++ [token])
                                       else popOperators stack res token

executeAlt :: [Automat] -> [Automat]
executeAlt (a1:a2:stack) = (alternativeNFA a1 a2) : stack

executeIter :: [Automat] -> [Automat]
executeIter (a:stack) = (iterateNFA a) : stack

executeOperator :: [Automat] -> OperatorType -> [Automat]
executeOperator a operator =
    if operator == Plus then executeAlt a
    else if operator == Star then executeIter a
    else a
    
    
concatAutomatStack :: [Automat] -> Automat
concatAutomatStack [a] = a
concatAutomatStack (a:as) = concatNFA (concatAutomatStack as) a

createNFA :: [Token] -> Automat
createNFA input = createNFAInner input []
  where
    createNFAInner :: [Token] -> [Automat] -> Automat
    createNFAInner [] a = concatAutomatStack a
    createNFAInner ((tokenType, tokenStr):xs) a =
      if tokenType == Literal then createNFAInner xs (literalAutomat:a)
      else if tokenType == Operator then createNFAInner xs $ executeOperator a operatorType
      else error("Wrong token type in token input")
      where
        literalAutomat = stringNFA tokenStr
        (operatorType, _) = getOperator $ head tokenStr

regex :: String -> String -> Bool
regex r input = testNFA nfa input
  where
    tokens = parse r "ab" -- queue to stack
    nfa = createNFA tokens