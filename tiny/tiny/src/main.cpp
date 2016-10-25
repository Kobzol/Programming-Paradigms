# include <stdio.h>

int yyparse();

int main(int argc, char** argv) 
{
   extern int yydebug;
   extern int numErrors;
   extern FILE* yyin;
//   yydebug = 1;
   if( argc > 1 ) {
      yyin = fopen(argv[1], "r");
      if( yyin == NULL ) {
         perror(argv[1]);
         return 1;
      }
   }
   yyparse();
   return numErrors > 0;
} // main
