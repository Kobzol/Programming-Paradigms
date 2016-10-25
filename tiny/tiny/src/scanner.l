%{

# include <ctype.h>
# include <stdlib.h>
# include <string.h>

static int searchKwd(const char* name);
%}

ws			[ \n\t]+
digit		[0-9]
alpha		[a-zA-Z]
alnum		[a-zA-Z0-9_]

%%
{ws}					;
"//"					{ int ch; while( (ch = yyinput()) != EOF && ch != '\n' ) ; }
{digit}+				{ yylval.num = atoi(yytext); return S_NUM; }
{alpha}{alnum}*	{ return searchKwd(yytext); }
"=="					{ yylval.op = O_EQU; return S_RELOP; }
"<="					{ yylval.op = O_LEQ; return S_RELOP; }
">="					{ yylval.op = O_GEQ; return S_RELOP; }
"!="					{ yylval.op = O_NEQ; return S_RELOP; }
"<"					{ yylval.op = O_LTH; return S_RELOP; }
">"					{ yylval.op = O_GTH; return S_RELOP; }
"+"					{ yylval.op = O_ADD; return S_ADDOP; }
"-"					{ yylval.op = O_SUB; return S_ADDOP; }
"*"					{ yylval.op = O_MUL; return S_MULOP; }
"/"					{ yylval.op = O_DIV; return S_MULOP; }
"%"					{ yylval.op = O_MOD; return S_MULOP; }
"&&"					{ return S_ANDAND; }
"||"					{ return S_OROR; }
.						{ return yytext[0]; }

%%

struct Kwd { 
   char*    name;
   int		symbol;
};

static Kwd kwdTable[] = {
	"do",		S_DO,
	"else",  	S_ELSE,
	"eof",		S_EOF,
        "for",          S_FOR,
	"if",    	S_IF,
	"read",  	S_READ,
	"repeat",	S_REPEAT,
	"until",	S_UNTIL,
	"while",	S_WHILE,
	"write",	S_WRITE,
	NULL,    	S_ID,
};

static int searchKwd(const char* name)
{
   for(Kwd *p = kwdTable; p->name != NULL; p++) {
      if( strcasecmp(p->name, name) == 0 )
		   return p->symbol;
   }
	strcpy(yylval.id = new char[strlen(name)+1], name);
	return S_ID;
} // searchKwd

int yywrap()
{
	return 1;
}
