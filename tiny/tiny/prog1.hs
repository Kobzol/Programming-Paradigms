import Tiny

prog :: Prog
prog =
   (Seq 
     (Eval 
       (Asgn "i" 
         (Num 1)))
     (While 
       (Lth 
         (Var "i")
         (Num 10))
       (Seq 
         (Write 
           (Var "i"))
         (Eval 
           (Asgn "i" 
             (Add 
               (Var "i")
               (Num 1)))))))
