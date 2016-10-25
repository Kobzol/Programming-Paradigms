import Tiny1

prog :: Prog
prog =
   (Seq 
     (Eval 
       (Asgn "max" 
         Read))
     (Seq 
       (Eval 
         (Asgn "n" 
           Read))
       (Seq 
         (While 
           (Neq 
             (Var "n")
             (Num 0))
           (Seq 
             (If 
               (Gth 
                 (Var "n")
                 (Var "max"))
               (Eval 
                 (Asgn "max" 
                   (Var "n")))
               Skip)
             (Eval 
               (Asgn "n" 
                 Read))))
         (Write 
           (Var "max")))))
