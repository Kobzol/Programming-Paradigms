import Tiny

prog :: Prog
prog =
   (Seq 
     (Eval 
       (Asgn "max" 
         Read))
     (Seq 
       (While 
         (Not 
           Eof)
         (Seq 
           (Eval 
             (Asgn "n" 
               Read))
           (If 
             (Gth 
               (Var "n")
               (Var "max"))
             (Eval 
               (Asgn "max" 
                 (Var "n")))
             Skip)))
       (Write 
         (Var "max"))))
