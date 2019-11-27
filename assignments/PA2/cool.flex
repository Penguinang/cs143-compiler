/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
// #define yylval cool_yylval
// #define yylex  cool_yylex

#define cool_yylex  yylex
extern "C" int yylex (void);

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
DIGIT           [0-9]
ZERO            0
UNUMBER         [1-9]{DIGIT}*|{ZERO}
SNUMBER         (\+|-){UNUMBER}
INTNUM          {UNUMBER}|{SNUMBER}
NEWLINE         \n
TAB             \t
WHITESPACE      [\r\t\v\f ]
LCHARACTER      [a-z]
UCHARACTER      [A-Z]
CHARACTER       {LCHARACTER}|{UCHARACTER}

IDBODY          ({CHARACTER}|_|{DIGIT})*
PCLASSID        {UCHARACTER}{IDBODY}
POBJECTID       {LCHARACTER}{IDBODY}

%x comment string
%%

 /*
  *  Nested comments
  */
{NEWLINE}       { ++curr_lineno; }
{WHITESPACE}    { }
{INTNUM}        { 
    cool_yylval.symbol = inttable.add_string(yytext);
    return (INT_CONST); }

"(*"            { BEGIN(comment); }
<comment>{
"*)"            { BEGIN(INITIAL); }
{NEWLINE}       { ++curr_lineno; }
.               { }
}
"*)"            { printf("error: unmatched right comment"); }

"\""            { BEGIN(string); }
<string>{
\\\"            { strcat(string_buf, yytext); }
\\n            { strcat(string_buf, "\n"); }
\"         { 
    BEGIN(INITIAL); 
    cool_yylval.symbol = stringtable.add_string(string_buf);
    memset(string_buf, 0, MAX_STR_CONST);
    return (STR_CONST);
     }
.               { strcat(string_buf, yytext); }

}

"class"         { return (CLASS); }
"inherits"      { return (INHERITS); }

"if"            { return (IF); }
"then"          { return (THEN); }
"else"          { return (ELSE); }
"fi"            { return (FI); }
"loop"          { return (LOOP); }
"pool"          { return (POOL); }
"while"         { return (WHILE); }
"of"            { return (OF); }
"case"          { return (CASE); }
"esac"          { return (ESAC); }
"let"           { return (LET); }
"new"           { return (NEW); }
"<-"            { return (ASSIGN); }
"isvoid"        { return (ISVOID); }
"not"           { return (NOT); }
"in"            { return (IN); }
"+"             { return ('+'); }
"/"             { return ('/'); }
"-"             { return ('-'); }
"*"             { return ('*'); }
"="             { return ('='); }
"<"             { return ('<'); }
"."             { return ('.'); }
"~"             { return ('~'); }
","             { return (','); }
";"             { return (';'); }
":"             { return (':'); }
"("             { return ('('); }
")"             { return (')'); }
"@"             { return ('@'); }
"{"             { return ('{'); }
"}"             { return ('}'); }


{POBJECTID}     { 
    cool_yylval.symbol = idtable.add_string(yytext); 
    return (OBJECTID); }
{PCLASSID}       { 
    cool_yylval.symbol = idtable.add_string(yytext); 
    return (TYPEID); }


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

    /*
.               { printf("\nerror: unmatched %s\n", yytext); exit(-1); }
    */

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
