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
#define yylval cool_yylval
#define yylex  cool_yylex

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

extern int curr_lineno; // 当前的行号
extern int verbose_flag;
extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}
%option noyywrap
/*
 * Define names for regular expressions here.
 */
digit       [0-9]
capital     [A-Z]
lowcase     [a-z]
letter      {capital}|{lowcase}
lcomment    "(*"
rcomment    "*)"
commentele  .|"\n"
/* keyword */
WHITE           ([^ \t\n\r\a]+)
CLASS           [cC][lL][aA][sS][sS]
ELSE            [eE][lL][sS][eE]
FI              [fF][iI]
IF              [iI][fF]
IN              [iI][nN]
INHERITS        [iI][nN][hH][eE][rR][iI][tT][sS]
LET             [lL][eE][tT]
LOOP            [lL][oO][oO][pP]
POOL            [pP][oO][oO][lL]
THEN            [tT][hH][eE][hH]
WHILE           [wW][hH][iI][lL][eE]
CASE            [cC][aA][sS][eE]
ESAC            [eE][sS][aA][cC]
OF              [oO][fF]
NEW             [nN][eE][wW]
ISVOID          [iI][sS][vV][oO][iI][dD]
NOT             [nN][oO][tT]
TRUE_CONST      [t][rR][uU][eE]
FALSE_CONST     [f][aA][lL][sS][eE]
BOOL_CONST      {TRUE_CONST}|{FALSE_CONST}
/* operators*/
DARROW          =>
LE              <=
ASSIGN          <-
/* constant or identifier */
INT_CONST       [0-9][0-9]*
TYPEID          "Int"|"Bool"|"String"
OBJECTID        {capital}({letter}|{digit}|_)*
%%
 /*
  *  Nested comments 关于注释的处理,包含嵌套注释
  */

 /*
  *  The multiple-character operators. 各种运算符的定义和匹配
  */
{DARROW} {
    return (DARROW);
}
{LE} {
    return (LE);
}
{ASSIGN} {
    return (ASSIGN);
}
 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS} {
return (CLASS);
}
{ELSE} {
return (ELSE);
}
{FI} {
return (FI);
}
{IF} {
return (IF);
}
{IN} {
return (IN);
}
{INHERITS} {
return (INHERITS);
}
{LET} {
return (LET);
}
{LOOP} {
return (LOOP);
}
{POOL} {
return (POOL);
}
{THEN} {
return (THEN);
}
{WHILE} {
return (WHILE);
}
{CASE} {
return (CASE);
}
{ESAC} {
return (ESAC);
}
{OF} {
return (OF);
}
{NEW} {
return (NEW);
}
{ISVOID} {
return (ISVOID);
}
{NOT} {
    return (NOT);
}
 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
{WHITE};
"\n" {
    curr_lineno += 1;
}

[0-9][0-9]* {
    return (INT_CONST);
}
{BOOL_CONST} {
return (BOOL_CONST);
}
{TYPEID} {
return (TYPEID);
}
{OBJECTID} {
return (OBJECTID);
}
<<EOF>> {
return 0;
}

%%
