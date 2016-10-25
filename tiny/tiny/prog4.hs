import Tiny

prog :: Prog
prog =
   (Seq 
     (Eval 
       (Asgn "a" 
         Read))
     (Seq 
       (Eval 
         (Asgn "b" 
           Read))
       (Seq 
         (Write 
           (And 
             (Var "a")
             (Var "b")))
         (Seq 
           (Write 
             (Or 
               (Var "a")
               (Var "b")))
           (Write 
             (Not 
               (Var "a")))))))
