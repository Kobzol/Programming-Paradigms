module Tiny1 (
   Exp(..), Com(..), State(..),  -- exportovane konstruktory
   Prog, Store, Input, Output,   -- exportovane typy
   e, c, p                       -- exportovane funkce
)
where

data Exp = Add   Exp Exp      -- e + e
         | Sub   Exp Exp      -- e - e
         | Mul   Exp Exp      -- e * e
         | Div   Exp Exp      -- e / e
         | Mod   Exp Exp      -- e % e
         | Neg   Exp          -- -e
         | Equ   Exp Exp      -- e == e
         | Neq   Exp Exp      -- e != e
         | Lth   Exp Exp      -- e < e
         | Gth   Exp Exp      -- e > e
         | Leq   Exp Exp      -- e <= e
         | Geq   Exp Exp      -- e >= e
         | Asgn  String Exp   -- id = e
         | Cond  Exp Exp Exp  -- e ? e : e
         | Read               -- read
         | Num   Int          -- num
         | Var   String       -- id

data Com = Eval  Exp          -- e;
         | If    Exp Com Com  -- if( e ) c else c
         | While Exp Com      -- while( e ) c
         | Do    Exp Com      -- do c while( e );
         | Write Exp          -- write e;
         | Seq   Com Com      -- c ; c
         | Skip               -- ;

type Store  = String -> Maybe Int
type Input  = [Int]

data Value = I Int
           | OK
           | Err String
type Output = [Value]

data State  = State {store::Store, input::Input}

type CCont = State -> Output
type ECont = Int -> State -> Output

instance Show Value where
   showsPrec p (I x)   = shows x
   showsPrec p OK      = ("OK" ++)
   showsPrec p (Err e) = (e ++)

--------------------------------
-- semantika vyrazu
--------------------------------

e :: Exp -> State -> ECont -> Output


evalBin :: (Int -> Int -> Int) -> Exp -> Exp -> State -> ECont -> Output
evalBin op e1 e2 s ec = e e1 s (\v1 s' ->
                          e e2 s' (\v2 s'' -> 
                            ec (op v1 v2) s''))

evalCond :: (Int -> Int -> Bool) -> Exp -> Exp -> State -> ECont -> Output
evalCond op e1 e2 s ec = e e1 s (\v1 s' ->
                           e e2 s' (\v2 s'' ->
                             ec (if op v1 v2 then 1 else 0) s''))

e (Add e1 e2) s ec = evalBin (+) e1 e2 s ec
e (Sub e1 e2) s ec = evalBin (-) e1 e2 s ec
e (Mul e1 e2) s ec = evalBin (*) e1 e2 s ec
e (Div e1 e2) s ec = evalBin div e1 e2 s ec
e (Mod e1 e2) s ec = evalBin mod e1 e2 s ec

e (Equ e1 e2) s ec = evalCond (==) e1 e2 s ec
e (Neq e1 e2) s ec = evalCond (/=) e1 e2 s ec
e (Lth e1 e2) s ec = evalCond (<)  e1 e2 s ec
e (Gth e1 e2) s ec = evalCond (>)  e1 e2 s ec
e (Leq e1 e2) s ec = evalCond (<=) e1 e2 s ec
e (Geq e1 e2) s ec = evalCond (>=) e1 e2 s ec

e (Asgn v e1) s ec = e e1 s (\v1 s' ->
                       ec v1 (State 
                          (\v' -> if v'==v then (Just v1) else (store s') v') 
                          (input s'))) 

e (Cond b e1 e2) s ec = e b s (\v1 s' ->
                          if v1 == 0 then e e2 s' ec
                                     else e e1 s' ec)

e Read (State s (x:i)) ec = ec x (State s i)
e Read (State _ []) _     = [Err "Read: prazdny vstup"]

e (Num x) s ec = ec x s

e (Var v) s ec = case (store s) v of
                    Just v1 -> ec v1 s
                    Nothing -> [Err ("Nedefinovana promenna: "++v)]

--------------------------------
-- semantika prikazu
--------------------------------

c :: Com -> State -> CCont -> Output

c (Eval e1) s cc = e e1 s (\_ s' -> cc s')

c (If b c1 c2) s cc = e b s (\v1 s' ->
                        if v1 == 0 then c c2 s' cc
                        else c c1 s' cc)

c (While b c1) s cc = e b s (\v1 s' ->
                        if v1 == 0 then cc s'
                        else c c1 s' (\s'' -> c (While b c1) s'' cc))

c (Seq c1 c2) s cc = c c1 s (\s' -> c c2 s' cc)

c (Write e1) s cc = e e1 s (\v1 s' ->
                      (I v1):(cc (State (store s') (input s'))))

c Skip s cc = cc s

--------------------------------
-- semantika programu
--------------------------------

type Prog = Com

p :: Prog -> Input -> Output

emptyStore :: Store
emptyStore id = Nothing

initialState :: Input -> State
initialState inp = State {store=emptyStore, input=inp}

p body inp = c body (initialState inp) (\_ -> [OK]) 

------------------------------------------------------------------------
