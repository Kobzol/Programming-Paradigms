import Tiny

prog :: Prog
prog =
   (For 
     (Asgn "i" 
       (Num 0))
     (Lth 
       (Var "i")
       (Num 10))
     (Asgn "i" 
       (Add 
         (Var "i")
         (Num 1)))
     (Write 
       (Var "i")))
