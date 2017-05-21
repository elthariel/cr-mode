%{
#include <iostream>
using namespace std;

#define YY_DECL extern "C" int yylex()

#include "crystal.tab.h"

#define TOKEN(id) return t##id
#define KEYWORD(id) return k##id

%}

%%
#.*$            { TOKEN(COMMENT); }

[0-9]+[.][0-9]+([eE][-+]?[0-9]+)?(_f(32|64))? { TOKEN(NUMBER); }
[0-9]+([eE][-+]?[0-9]+)?(_([ui](8|16)|[uif](32|64)))? { TOKEN(NUMBER); }
0x[0-9a-fA-F]+(_([ui](8|16)|[uif](32|64)))? { TOKEN(NUMBER); }
0o[0-7]+(_([ui](8|16)|[uif](32|64)))? { TOKEN(NUMBER); }

\'([^\']|\\.)*\'                { TOKEN(CHAR); }
:[a-zA-Z0-9_]+[?!]?             { TOKEN(SYM); }
\"([^\"]|\\.)*\"                { TOKEN(STR); }

[A-Z][a-zA-Z0-9_]*              { TOKEN(CID); }
[a-z_][a-zA-Z0-9_]*[!?]?        { TOKEN(ID); }

[ \t\n] ;
.* { cout << "Unable to parse: " << yytext << endl; exit(42); }

%%

void yyerror(char const *s) {
  cout << s << "Unexpected \"" << yytext << "\" on line " << yylineno << endl;
  exit(-1);
}

/* int main(int, char**) { */
/*     // lex through the input: */
/*     yylex(); */
/* } */
