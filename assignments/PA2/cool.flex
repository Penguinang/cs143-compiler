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
int comment_layer = 0;

#define CHECK_BUF_LEN \
{ \
    if(strlen(string_buf) + 2 > MAX_STR_CONST) { \
            cool_yylval.error_msg = "String constant too long";  \
            memset(string_buf, 0, MAX_STR_CONST); \
            BEGIN(string_error); \
            return (ERROR); \
        } \
}

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
INTNUM         {DIGIT}+
NEWLINE         \n
TAB             \t
WHITESPACE      [\r\t\v\f ]
LCHARACTER      [a-z]
UCHARACTER      [A-Z]
CHARACTER       {LCHARACTER}|{UCHARACTER}

IDBODY          ({CHARACTER}|_|{DIGIT})*
PCLASSID        {UCHARACTER}{IDBODY}
POBJECTID       {LCHARACTER}{IDBODY}

%x comment comment_dash string string_error
%%

 /*
  *  Nested comments
  */
{NEWLINE}       { ++curr_lineno; }
{WHITESPACE}    { }
{INTNUM}        { 
    cool_yylval.symbol = inttable.add_string(yytext);
    return (INT_CONST); }

"(*"            { 
    BEGIN(comment); 
    ++ comment_layer;
}
<comment>{
"\\(*"          { }
"\\*)"          { }

"(*"            { 
    ++ comment_layer;
}
"*)"            { 
    if(--comment_layer == 0)
        BEGIN(INITIAL); 
}
{NEWLINE}       { ++curr_lineno; }
<<EOF>>         {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in comment"; 
    return (ERROR); 
}
.               { }
}

--              { BEGIN(comment_dash); }
<comment_dash>{
.               { }
{NEWLINE}              { ++curr_lineno; BEGIN(INITIAL); }
}

"*)"            { 
    cool_yylval.error_msg = "Unmatched *)"; 
    return (ERROR);
}

"\""            { BEGIN(string); }
<string>{
\\\"            { 
    CHECK_BUF_LEN;
    strcat(string_buf, yytext+1); }
\\b             { 
    CHECK_BUF_LEN;
    strcat(string_buf, "\b"); }
\\t             { 
    CHECK_BUF_LEN;
    strcat(string_buf, "\t"); }
\\n             { 
    CHECK_BUF_LEN;
    strcat(string_buf, "\n"); }
\\f             { 
    CHECK_BUF_LEN;
    strcat(string_buf, "\f"); }
\\\0              { 
    cool_yylval.error_msg = "String contains escaped null character."; 
    memset(string_buf, 0, MAX_STR_CONST);
    BEGIN(string_error);
    return (ERROR);
}
\0 {
    cool_yylval.error_msg = "String contains null character."; 
    memset(string_buf, 0, MAX_STR_CONST);
    BEGIN(string_error);
    return (ERROR);
}
\\\\            { 
    CHECK_BUF_LEN;
    strcat(string_buf, "\\"); }
\\.             { 
    CHECK_BUF_LEN;
    strcat(string_buf, yytext+1); }

\\\n            { 
    
    CHECK_BUF_LEN;
    strcat(string_buf, "\n");
    ++curr_lineno;
    }
\n              { 
    cool_yylval.error_msg = "Unterminated string constant"; 
    BEGIN(INITIAL); 
    memset(string_buf, 0, MAX_STR_CONST);
    ++curr_lineno;
    return (ERROR);
    }
<<EOF>>         {
    BEGIN(INITIAL); 
    cool_yylval.error_msg = "EOF in string constant"; 
    return (ERROR);
}

\"         { 
    cool_yylval.symbol = stringtable.add_string(string_buf);
    BEGIN(INITIAL); 
    memset(string_buf, 0, MAX_STR_CONST);
    return (STR_CONST);
     }


.               { 
    CHECK_BUF_LEN;
    strcat(string_buf, yytext); }

}

<string_error>{
\\\"            { }
\"  {
    BEGIN(INITIAL);
}
\n  {
    BEGIN(INITIAL);
}
.   {}
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
    cool_yylval.error_msg = yytext;
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
