%{
# pragma warning( disable : 4102 )  
# include "defs.h"
# include "tree.h"
# include <stdlib.h>

int numErrors = 0;
SkipStmt* skipStmt = new SkipStmt();
%}

%union {
   Oper		op;
	char*		id;
	int		num;
	Expr*		e;
	Stmt*		s;
	struct { Stmt* first; Stmt* last; } list;
}

%term S_DO S_ELSE S_EOF S_IF S_READ S_REPEAT S_UNTIL S_WHILE S_WRITE S_FOR

%term <id>  S_ID
%term <num> S_NUM

%right '='
%right ':' '?'
%left S_OROR
%left S_ANDAND
%left '!'
%nonassoc <op> S_RELOP
%left <op> S_ADDOP
%left <op> S_MULOP
%left S_UNARY

%type <e> expr
%type <s> stmt
%type <list> stmts
%%

program:
	stmts
	{ if( numErrors == 0 ) $1.first->dumpTree(); }

stmts: 
	stmts stmt
	{ if( $2 == NULL ) $$ = $1;
	  else if( $1.first == NULL ) $$.first = $$.last = $2;
	  else if( $1.last->op != C_SEQ ) { 
	    $$.first = $$.last = new SeqStmt($1.first, $2); 
	  } else { 
		 SeqStmt* last = static_cast<SeqStmt*>($1.last);
	    $$.first = $1.first; 
		 $$.last = last->right = new SeqStmt(last->right, $2);
	  }
	}
|	/* e */
	{ $$.first = $$.last = NULL; }

stmt:
	expr ';' 
	{ $$ = new UnaryStmt(C_EVAL, $1); }
|	S_WRITE expr ';'
	{ $$ = new UnaryStmt(C_WRITE, $2); }
|	S_IF '(' expr ')' stmt
	{ $$ = new IfStmt($3, $5, skipStmt); }
|  S_IF '(' expr ')' stmt S_ELSE stmt
	{ $$ = new IfStmt($3, $5, $7); }
|  S_WHILE '(' expr ')' stmt
	{ $$ = new LoopStmt(C_WHILE, $3, $5); }
|  S_REPEAT stmt S_UNTIL '(' expr ')' ';'
        { $$ = new LoopStmt(C_REPEAT, $5, $2); }
|  S_DO stmt S_WHILE '(' expr ')' ';'
	{ $$ = new LoopStmt(C_DO, $5, $2); }
|  S_FOR '(' expr ';' expr ';' expr ')' stmt
        { $$ = new ForStmt($3, $5, $7, $9); }
|  '{' stmts '}'
	{ $$ = $2.first; }
|  ';'
   { $$ = skipStmt; }
|  error ';'
   { $$ = skipStmt; }

expr:
   expr S_ADDOP expr { $$ = new BinaryExpr($2, $1, $3); }
|  expr S_MULOP expr { $$ = new BinaryExpr($2, $1, $3); }
|  expr S_RELOP expr { $$ = new BinaryExpr($2, $1, $3); }
|  expr S_ANDAND expr { $$ = new BinaryExpr(O_AND, $1, $3); }
|  expr S_OROR expr { $$ = new BinaryExpr(O_OR, $1, $3); }
|  '!' expr { $$ = new UnaryExpr(O_NOT, $2); }
|  S_EOF { $$ = new EofExpr; }
|  S_ID '=' expr { $$ = new AsgnExpr($1, $3); }
|  S_ADDOP expr %prec S_UNARY { $$ = $1 == O_ADD ? $2 : new UnaryExpr(O_NEG, $2); }
|  '(' expr ')' { $$ = $2; }
|  expr '?' expr ':' expr { $$ = new CondExpr($1, $3, $5); }
|  S_READ { $$ = new ReadExpr(); }
|  S_ID { $$ = new VarExpr($1); }
|  S_NUM { $$ = new ConstExpr($1); }


%%

# include "scanner.cpp"

void yyerror(char* msg)
{
   extern int yylineno;
   fprintf(stderr, "Line %d: %s\n", yylineno, msg);
	numErrors++;
} // yyerror

