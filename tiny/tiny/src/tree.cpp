# include <stdio.h>
# include "tree.h"

FILE* out = stdout;

static char* opname[] = {
   "Add", "Sub", "Mul", "Div", "Mod", "Neg",
   "Equ", "Neq", "Lth", "Gth", "Leq", "Geq",
   "Asgn", "Cond", "Read", "Var", "Num",
   "And", "Or", "Not", "Eof",
};

static char* comname[] = {
   "Eval", "If", "While", "Do", "Write", "Seq", "Repeat", "For"
};

static void lvl(int level) {
   fprintf(out, "\n%*c", level*2 + 1, ' ');
} // lvl

// =======
// Expr
// =======

void ReadExpr::dump(int level)
{
   fprintf(out, "Read");
} // ReadExpr::dump

void EofExpr::dump(int level)
{
   fprintf(out, "Eof");
} // EofExpr::dump

void VarExpr::dump(int level)
{
   fprintf(out, "(Var \"%s\")", var);
} // VarExpr::dump

void ConstExpr::dump(int level)
{
   fprintf(out, "(Num %d)", value);
} // ConstExpr::dump

void UnaryExpr::dump(int level)
{
   fprintf(out, "(%s ", opname[op]);
   lvl(level);
   left->dump(level+1);
   fprintf(out, ")");   
} // UnaryExpr::dump

void BinaryExpr::dump(int level)
{
   fprintf(out, "(%s ", opname[op]);
   lvl(level);
   left->dump(level+1);
   lvl(level);
   right->dump(level+1);
   fprintf(out, ")");
} // BinaryExpr::dump

void AsgnExpr::dump(int level)
{
   fprintf(out, "(Asgn \"%s\" ", var);
   lvl(level);
   right->dump(level+1);
   fprintf(out, ")");
} // AsgnExpr::dump

void CondExpr::dump(int level)
{
   fprintf(out, "(Cond ");
   lvl(level);
   cond->dump(level+1);
   lvl(level);
   left->dump(level+1);
   lvl(level);
   right->dump(level+1);
   fprintf(out, ")");
} // CondExpr::dump

// =======
// Stmt
// =======

void Stmt::dumpTree() 
{
   fprintf(out, "import Tiny\n\n");
   fprintf(out, "prog :: Prog\n");
   fprintf(out, "prog =");
   lvl(1);
   if( this == NULL )
      fprintf(out, "Skip");
   else
      dump(2);
	fprintf(out, "\n");
} // Stmt::dumpTree

void SkipStmt::dump(int level)
{
   fprintf(out, "Skip");
} // SkipStmt::dump

void UnaryStmt::dump(int level)
{
   fprintf(out, "(%s ", comname[op]);
   lvl(level);
   arg->dump(level+1);
   fprintf(out, ")");
} // UnaryStmt::dump

void IfStmt::dump(int level)
{
   fprintf(out, "(If ");
   lvl(level);
   cond->dump(level+1);
   lvl(level);
   left->dump(level+1);
   lvl(level);
   right->dump(level+1);
   fprintf(out, ")");
} // IfStmt::dump

void LoopStmt::dump(int level)
{
   fprintf(out, "(%s ", comname[op]);
   lvl(level);
   cond->dump(level+1);
   lvl(level);
   body->dump(level+1);
   fprintf(out, ")");
} // LoopStmt::dump

void ForStmt::dump(int level)
{
   fprintf(out, "(For ");
   lvl(level);
   init->dump(level+1);
   lvl(level);
   cond->dump(level+1);
   lvl(level);
   incr->dump(level+1);
   lvl(level);
   body->dump(level+1);
   fprintf(out, ")");
} // ForStmt::dump

void SeqStmt::dump(int level)
{
   fprintf(out, "(Seq ");
   lvl(level);
   left->dump(level+1);
   lvl(level);
   right->dump(level+1);
   fprintf(out, ")");
} // SeqStmt::dump

