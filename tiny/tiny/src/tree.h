enum Oper {
   O_ADD, O_SUB, O_MUL, O_DIV, O_MOD, O_NEG,
   O_EQU, O_NEQ, O_LTH, O_GTH, O_LEQ, O_GEQ,
   O_ASGN, O_COND, O_READ, O_VAR, O_CONST,
   O_AND, O_OR, O_NOT, O_EOF, 
};

class Expr {
public:
   Oper op;
   Expr(Oper anOp)
      : op(anOp) {}
   virtual void dump(int level) = 0;
}; // Expr

class ReadExpr: public Expr {
public:
   ReadExpr()
      : Expr(O_READ) {}
   virtual void dump(int level);
}; // ReadExpr

class EofExpr: public Expr {
public:
   EofExpr()
      : Expr(O_EOF) {}
   virtual void dump(int level);
}; // EofExpr

class VarExpr: public Expr {
public:
   const char* var;
   VarExpr(const char* aVar)
      : Expr(O_VAR), var(aVar) {}
   virtual void dump(int level);
}; // VarExpr

class ConstExpr: public Expr {
public:
   int value;
   ConstExpr(int aValue)
      : Expr(O_CONST), value(aValue) {}
   virtual void dump(int level);
}; // ConstExpr

class UnaryExpr: public Expr {
public:
   Expr*    left;
   UnaryExpr(Oper anOp, Expr* aLeft)
      : Expr(anOp), left(aLeft) {}
   virtual void dump(int level);
}; // UnaryExpr

class BinaryExpr: public Expr {
public:
   Expr*    left;
   Expr*    right;
   BinaryExpr(Oper anOp, Expr* aLeft, Expr* aRight)
      : Expr(anOp), left(aLeft), right(aRight) {}
   virtual void dump(int level);
}; // BinaryExpr

class AsgnExpr: public Expr {
public:
   const char* var;
   Expr*       right;
   AsgnExpr(const char* aVar, Expr* aRight)
      : Expr(O_ASGN), var(aVar), right(aRight) {}
   virtual void dump(int level);
}; // AsgnExpr

class CondExpr: public Expr {
public:
   Expr*    cond;
   Expr*    left;
   Expr*    right;
   CondExpr(Expr* aCond, Expr* aLeft, Expr* aRight)
      : Expr(O_COND), cond(aCond), left(aLeft), right(aRight) {}
   virtual void dump(int level);
}; // CondExpr


enum Comm {
   C_EVAL, C_IF, C_WHILE, C_DO, C_WRITE, C_SEQ, C_REPEAT, C_SKIP, C_FOR,
}; // Comm

class Stmt {
public:
   Comm     op;
   Stmt(Comm anOp): op(anOp) {}
   virtual void dump(int level) = 0;
   void dumpTree();
}; // Stmt

class SkipStmt: public Stmt {
public:
   SkipStmt(): Stmt(C_SKIP) {}
   virtual void dump(int level);
}; // SkipStmt

class UnaryStmt: public Stmt {
public:
   Expr*    arg;
   UnaryStmt(Comm anOp, Expr* anArg)
      : Stmt(anOp), arg(anArg) {}
   virtual void dump(int level);
}; // UnaryStmt

class IfStmt: public Stmt {
public:
   Expr*    cond;
   Stmt*    left;
   Stmt*    right;
   IfStmt(Expr* aCond, Stmt* aLeft, Stmt* aRight)
      : Stmt(C_IF), cond(aCond), left(aLeft), right(aRight) {}
   virtual void dump(int level);
}; // IfStmt

class LoopStmt: public Stmt {
public:
   Expr*    cond;
   Stmt*    body;
   LoopStmt(Comm anOp, Expr* aCond, Stmt* aBody)
      : Stmt(anOp), cond(aCond), body(aBody) {}
   virtual void dump(int level);
}; // LoopStmt

class ForStmt: public Stmt {
public:
  Expr*     init;
  Expr*     cond;
  Expr*     incr;
  Stmt*     body;
  ForStmt(Expr* aInit, Expr* aCond, Expr* aIncr, Stmt* aBody)
     : Stmt(C_FOR), init(aInit), cond(aCond), incr(aIncr), body(aBody) {}
  virtual void dump(int level);
};

class SeqStmt: public Stmt {
public:
   Stmt*    left;
   Stmt*    right;
   SeqStmt(Stmt* aLeft, Stmt* aRight)
      : Stmt(C_SEQ), left(aLeft), right(aRight) {}
   virtual void dump(int level);
}; // SeqStmt
