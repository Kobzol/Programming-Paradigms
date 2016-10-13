module Cviko5 where

import Prelude

-- typové konstruktory typu Data
data Expr = Num (Int)
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
          deriving (Eq)

expr1 = Mul (Add (Var 'x') (Num 2)) (Mul (Num 3) (Var 'x'))

eval :: Expr -> Int
eval (Num a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Div a b) = (eval a) `div` (eval b)


makeBrace :: Int -> Int -> String -> String
makeBrace prec prev brace = if prec < prev then brace
                            else ""

showExpr :: Expr -> Int -> String
showExpr (Num x) _ = show x
showExpr (Var x) _ = [x]
showExpr (Add a b) prec = (makeBrace 1 prec "(") ++ (showExpr a 1) ++ "+" ++ (showExpr b 1) ++ (makeBrace 1 prec ")")
showExpr (Sub a b) prec = (makeBrace 1 prec "(") ++ (showExpr a 1) ++ "-" ++ (showExpr b 1) ++ (makeBrace 1 prec ")")
showExpr (Mul a b) prec = (makeBrace 2 prec "(") ++ (showExpr a 2) ++ "*" ++ (showExpr b 2) ++ (makeBrace 2 prec ")")
showExpr (Div a b) prec = (makeBrace 2 prec "(") ++ (showExpr a 2) ++ "/" ++ (showExpr b 2) ++ (makeBrace 2 prec ")")


deriv :: Expr -> Char -> Expr
deriv (Num x) _ = Num 0
deriv (Var x) y = if x == y then Num 1
                  else Num 0
deriv (Add a b) x = Add (deriv a x) (deriv b x)
deriv (Sub a b) x = Sub (deriv a x) (deriv b x)
deriv (Mul a b) x = Add (Mul (deriv a x) b) (Mul a (deriv b x))
deriv (Div a b) x = Div (Sub (Mul (deriv a x) b) (Mul a (deriv b x))) (Mul b b)


simplify :: Expr -> Expr
simplify (Add (Num a) (Num b)) = Num (a + b)
simplify (Sub (Num a) (Num b)) = Num (a - b)
simplify (Mul (Num a) (Num b)) = Num (a * b)
simplify (Div (Num a) (Num b)) = Num (a `div` b)
simplify (Mul a (Num 0)) = Num 0
simplify (Mul (Num 0) a) = Num 0
simplify (Add a (Num 0)) = simplify a
simplify (Add (Num 0) a) = simplify a
simplify (Mul a (Num 1)) = simplify a
simplify (Mul (Num 1) a) = simplify a
simplify (Add a b) = Add (simplify a) (simplify b)
simplify (Sub a b) = Sub (simplify a) (simplify b)
simplify (Mul a b) = Mul (simplify a) (simplify b)
simplify (Div a b) = Div (simplify a) (simplify b)
simplify a = a

simplifyRec :: Expr -> Expr -> Expr
simplifyRec now lst = if now == lst then now
                      else simplifyRec (simplify now) now

simp :: Expr -> Expr
simp a = simplifyRec a (Add a (Num 0))