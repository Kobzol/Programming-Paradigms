module Tiny (
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
         | Not   Exp          -- !e
         | And  Exp Exp      -- e && e
         | Or   Exp Exp      -- e || e
         | Equ   Exp Exp      -- e == e
         | Neq   Exp Exp      -- e != e
         | Lth   Exp Exp      -- e < e
         | Gth   Exp Exp      -- e > e
         | Leq   Exp Exp      -- e <= e
         | Geq   Exp Exp      -- e >= e
         | Asgn  String Exp   -- id = e
         | Cond  Exp Exp Exp  -- e ? e : e
         | Eof                -- end-of-file
         | Read               -- read
         | Num   Int          -- num
         | Var   String       -- id

data Com = Eval  Exp          -- e;
         | If    Exp Com Com  -- if( e ) c else c
         | While Exp Com      -- while( e ) c
         | Do    Exp Com      -- do c while( e );
         | For   Exp Exp Exp Com -- for (init; cond; after) body;
         | Write Exp          -- write e;
         | Seq   Com Com      -- c ; c
         | Skip               -- ;

type Store  = String -> Int
type Input  = [Int]
type Output = [Int]
data State  = State {store::Store, input::Input, output::Output}

-- pomocne
toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

toInt :: Bool -> Int
toInt False = 0
toInt True = 1


--------------------------------
-- semantika vyrazu
--------------------------------

e :: Exp -> State -> (Int, State)

evalBin :: (Int -> Int -> Int) -> Exp -> Exp -> State -> (Int, State)
evalBin op e1 e2 s = let (v1,s') = e e1 s 
                     in let (v2,s'') = e e2 s'
                        in ((op v1 v2), s'')

evalCond :: (Int -> Int -> Bool) -> Exp -> Exp -> State -> (Int, State)
evalCond op e1 e2 s = let (v1,s') = e e1 s 
                      in let (v2,s'') = e e2 s'
                         in (if op v1 v2 then 1 else 0, s'')
                         
evalBinBool :: (Bool -> Bool -> Bool) -> Exp -> Exp -> State -> (Int, State)
evalBinBool op e1 e2 s = let (v1,s') = e e1 s 
                     in let (v2,s'') = e e2 s'
                        in (toInt (op (toBool v1) (toBool v2)), s'')

e (Add e1 e2) s = evalBin (+) e1 e2 s
e (Sub e1 e2) s = evalBin (-) e1 e2 s
e (Mul e1 e2) s = evalBin (*) e1 e2 s
e (Div e1 e2) s = evalBin div e1 e2 s
e (Mod e1 e2) s = evalBin mod e1 e2 s

e (Not expr) s = (result, s')
  where
    (val, s') = e expr s
    b = toBool val
    result = toInt (not b)
    
e (And e1 e2) s = evalBinBool (&&) e1 e2 s
e (Or e1 e2) s = evalBinBool (||) e1 e2 s

e (Equ e1 e2) s = evalCond (==) e1 e2 s
e (Neq e1 e2) s = evalCond (/=) e1 e2 s
e (Lth e1 e2) s = evalCond (<)  e1 e2 s
e (Gth e1 e2) s = evalCond (>)  e1 e2 s
e (Leq e1 e2) s = evalCond (<=) e1 e2 s
e (Geq e1 e2) s = evalCond (>=) e1 e2 s

e (Asgn v e1) s = let (v1,(State st i o)) = e e1 s
                      s'' v' = if v'==v then v1
                               else st v'
                  in (v1, (State s'' i o))

e (Cond b e1 e2) s = let (v1,s') = e b s
                     in if v1 == 0 then e e2 s'
                                   else e e1 s'

e Read (State s (x:i) o) = (x, (State s i o))
e Read (State _ [] _) = error "Read: prazdny vstup"

e Eof (State s [] o) = (1, (State s [] o))
e Eof s = (0, s)

e (Num x) s = (x, s)

e (Var v) (State s i o) = (s v, (State s i o))

--------------------------------
-- semantika prikazu
--------------------------------
forInner :: Exp -> Exp -> Com -> State -> State
forInner cond after body s = if condEval == 0 then sEval
                             else forInner cond after body sAfter
  where
    (condEval, sEval) = e cond s
    sBody = c body sEval
    (_, sAfter) = e after sBody


c :: Com -> State -> State

c (Eval e1)  s = let (_,s') = e e1 s
                 in s'

c (If b c1 c2) s = let (v1,s') = e b s
                   in if v1 == 0 then c c2 s'
                      else c c1 s'

c (While b c1) s = let (v1,s') = e b s
                   in if v1 == 0 then s'
                    else c (While b c1) (c c1 s')

c (For start cond after body) s = forInner cond after body sInit
  where
    (_, sInit) = e start s

c (Seq c1 c2) s = c c2 (c c1 s)

c (Write e1) s = let (v1, (State s' i o)) = e e1 s
                 in (State s' i (o++[v1]))

c Skip s = s

--------------------------------
-- semantika programu
--------------------------------

type Prog = Com

p :: Prog -> Input -> Output

emptyStore :: Store
emptyStore id = error ("Store: Promenna "++id++" neni definovana")

initialState :: Input -> State
initialState inp = State {store=emptyStore, input=inp, output=[]}

p body inp = out 
             where State{output=out} = c body (initialState inp)

------------------------------------------------------------------------
