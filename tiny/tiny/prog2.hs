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
         (While 
           (Neq 
             (Var "a")
             (Var "b"))
           (If 
             (Gth 
               (Var "a")
               (Var "b"))
             (Eval 
               (Asgn "a" 
                 (Sub 
                   (Var "a")
                   (Var "b"))))
             (Eval 
               (Asgn "b" 
                 (Sub 
                   (Var "b")
                   (Var "a"))))))
         (Write 
           (Var "a")))))
