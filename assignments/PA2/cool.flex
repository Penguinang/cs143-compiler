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
SNUMBER         -{UNUMBER}
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

%x comment comment_dash string
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
<<EOF>>         {
    cool_yylval.error_msg = "Unterminated comment block"; 
    yyterminate();
    return (ERROR); 
}
.               { }
}

--              { BEGIN(comment_dash); }
<comment_dash>{
.               { }
{NEWLINE}              { ++curr_lineno; BEGIN(INITIAL); }
}

"*)"            { printf("error: unmatched right comment"); }

"\""            { BEGIN(string); }
<string>{
\\\"            { strcat(string_buf, yytext); }
\\b             { strcat(string_buf, "\b"); }
\\t             { strcat(string_buf, "\t"); }
\\n             { strcat(string_buf, "\n"); }
\\\n            { 
    strcat(string_buf, "\n");
    ++curr_lineno;
    }
\n              { 
    cool_yylval.error_msg = "A non-escaped newline character may not appear in a string, use \n or \\ instead"; 
    ++curr_lineno;
    return (ERROR); 
    }
<<EOF>>         {
    cool_yylval.error_msg = "Unterminated string literal"; 
    yyterminate();
    return (ERROR);
}
\\f             { strcat(string_buf, "\f"); }

\"         { 
    BEGIN(INITIAL); 
    cool_yylval.symbol = stringtable.add_string(string_buf);
    memset(string_buf, 0, MAX_STR_CONST);
    return (STR_CONST);
     }
.               { strcat(string_buf, yytext); }

}

(?i:class)         { return (CLASS); }
(?i:inherits)      { return (INHERITS); }
(?i:if)            { return (IF); }
(?i:then)          { return (THEN); }
(?i:else)          { return (ELSE); }
(?i:fi)            { return (FI); }
(?i:loop)          { return (LOOP); }
(?i:pool)          { return (POOL); }
(?i:while)         { return (WHILE); }
(?i:of)            { return (OF); }
(?i:case)          { return (CASE); }
(?i:esac)          { return (ESAC); }
(?i:let)           { return (LET); }
(?i:new)           { return (NEW); }
(?i:isvoid)        { return (ISVOID); }
(?i:not)           { return (NOT); }
(?i:in)            { return (IN); }
t(?i:rue)           { 
    cool_yylval.boolean = true;
    return (BOOL_CONST); }
f(?i:alse)           { 
    cool_yylval.boolean = false;
    return (BOOL_CONST); }
"<-"            { return (ASSIGN); }
"+"             { return ('+'); }
"/"             { return ('/'); }
"-"             { return ('-'); }
"*"             { return ('*'); }
"="             { return ('='); }
"<"             { return ('<'); }
"<="             { return (LE); }
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


.               { 
    cool_yylval.error_msg = "unrecognized token:";
    printf("\nerror: unmatched %s\n", yytext); 
    return (ERROR);
}


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
