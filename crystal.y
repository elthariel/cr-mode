%{
#include <cstdio>
#include <iostream>
using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
extern "C" int yyparse();
extern "C" FILE *yyin;

void yyerror(const char *s);
%}

/* %pure-parser */
/* %parse-param { parser_state *state } */
/* %lex-param { state } */

// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token tCOMMENT
%token tNUMBER
%token tCHAR
%token tSTR
%token tSYM
%token tID
%token tCID

%start program

%%
// this is the actual grammar that bison will parse, but for right now it's just
// something silly to echo to the screen what bison gets from flex.  We'll
// make a real one shortly:
program:        expressions { ; }

expressions:    expressions expression
        |       expression

expression:     value
                ;

value:          tCOMMENT { cout << "comment: " << $1 << endl; }
        |       tNUMBER { cout << "number: " << $1 << endl; }
        |       tCHAR { cout << "char: " << $1 << endl; }
        |       tSTR { cout << "str: " << $1 << endl; }
        |       tSYM { cout << "sym: " << $1 << endl; }
                ;

%%

int main(int ac, char **av) {
    // open a file handle to a particular file:
    FILE *myfile = fopen(av[1], "r");
    // make sure it is valid:
    if (!myfile) {
        cout << "I can't open " << av[1] << endl;
        return -1;
    }
    // set flex to read from it instead of defaulting to STDIN:
    yyin = myfile;

    // parse through the input until there is no more:
    do {
        yyparse();
    } while (!feof(yyin));

}

/* void yyerror(const char *s) { */
/*     cout << "EEK, parse error!  Message: " << s << endl; */
/* 	// might as well halt now: */
/*     exit(-1); */
/* } */
