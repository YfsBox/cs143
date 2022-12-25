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
#include <vector>
/* The compiler assumes these identifiers. */
#define yylval cool_yylval  // 用来描述第一行和最后一行的信息以及column的信息，这个是需要我们进行维护的
#define yylex  cool_yylex   // 一个函数
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
char *string_buf_ptr; // 用来组合字符串的常量
std::vector<char> string_vec;
static int nest_comment_cnt = 0;

extern int curr_lineno; // 当前的行号
extern int verbose_flag;
extern YYSTYPE cool_yylval;  // 用来表示当前的YYSTYPE
/*
 *  Add Your own definitions here
 */
%}

%option noyywrap
%x COMMENT
%x STRING
/*
 * Define names for regular expressions here.
 */
digit       [0-9]
capital     [A-Z]
lowcase     [a-z]
letter      {capital}|{lowcase}
/* keyword */
WHITE           [ \t\f\r\v]
CLASS           [cC][lL][aA][sS][sS]
ELSE            [eE][lL][sS][eE]
FI              [fF][iI]
IF              [iI][fF]
IN              [iI][nN]
INHERITS        [iI][nN][hH][eE][rR][iI][tT][sS]
LET             [lL][eE][tT]
LOOP            [lL][oO][oO][pP]
POOL            [pP][oO][oO][lL]
THEN            [tT][hH][eE][nN]
WHILE           [wW][hH][iI][lL][eE]
CASE            [cC][aA][sS][eE]
ESAC            [eE][sS][aA][cC]
OF              [oO][fF]
NEW             [nN][eE][wW]
ISVOID          [iI][sS][vV][oO][iI][dD]
NOT             [nN][oO][tT]
/* operators*/
DARROW          =>
LE              <=
ASSIGN          <-
/* constant or identifier */
INT_CONST       [0-9][0-9]*
TRUE_CONST      [t][rR][uU][eE]
FALSE_CONST     [f][aA][lL][sS][eE]
TYPEID          {capital}({letter}|{digit}|_)*
OBJECTID        {lowcase}({letter}|{digit}|_)*
/* comments,参考flex手册即可写个大概 */
COMMENT_BEGIN    "(*"
COMMENT_END    "*)"
COMMENT_LINE    --.*
QUOTATION    "\""

BACKSPACE   '\b'
TAB         '\t'
NEWLINE     '\n'
FORMFEED    '\f'
ESCAPE_SYM  \\.

%%
{COMMENT_LINE} {
}

<INITIAL>{COMMENT_BEGIN} {
    nest_comment_cnt++;
    BEGIN(COMMENT);
}
<COMMENT>{COMMENT_BEGIN} {
    nest_comment_cnt++;
}

<COMMENT>{COMMENT_END} {
    nest_comment_cnt--;
    if (nest_comment_cnt == 0) {
        BEGIN(INITIAL);
    }
}
<INITIAL>{COMMENT_END} {
    cool_yylval.error_msg = "find a commend_end error!";
    return (ERROR);
}

<COMMENT><<EOF>> {
    // std::cout << "in comment eof\n";
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    yyrestart(yyin);
    return (ERROR);
}

<COMMENT>"\n" {
    ++curr_lineno;
}

<COMMENT>[^\n(*)]* {}

<COMMENT>[(*)] {}

{QUOTATION} {
    yymore();
    BEGIN(STRING);
}

<STRING>{
    [^\\\"\n]* {
        yymore();
    }
    \\[^\n] {
        yymore();
    }
    \\\n {
        curr_lineno++;
        yymore();
    }


    \n {
        yylval.error_msg = "Unterminated string constant";
        BEGIN(INITIAL);
        curr_lineno++;
        return ERROR;
    }

    \\0 {
        yylval.error_msg = "Unterminated string constant";
        BEGIN 0;
        return ERROR;
    }

    \" {
        std::string str_const(yytext, yyleng);
        str_const = str_const.substr(1, yyleng - 2);

        std::string result = "";
        std::string::size_type pos;

        // 首先判断其中有没有\0
        if (str_const.find_first_of('\0') != std::string::npos) {
            yylval.error_msg = "String contains null character";
            BEGIN(INITIAL);
            return ERROR;
        }

        while ((pos = str_const.find_first_of("\\")) != std::string::npos) {
            result += str_const.substr(0, pos);

            switch (str_const[pos + 1]) {
            case 'b':
                result += "\b";
                break;
            case 't':
                result += "\t";
                break;
            case 'n':
                result += "\n";
                break;
            case 'f':
                result += "\f";
                break;
            default:
                result += str_const[pos + 1];
                break;
            }
            str_const = str_const.substr(pos + 2, str_const.length() - 2);
        }

        result += str_const;
        if (result.length() > 1024) {
            yylval.error_msg = "String constant too long";
            BEGIN(INITIAL);
            return ERROR;
        }

        cool_yylval.symbol = stringtable.add_string((char*)result.c_str());
        BEGIN(INITIAL);
        return STR_CONST;
    }

    <<EOF>> {
        cool_yylval.error_msg = "not find a quotation_end of string until eof";
        BEGIN(INITIAL);
        yyrestart(yyin);
        return (ERROR);
    }
}

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
{COMMENT_END} {
    yylval.error_msg = "Unmatched *)";
    return ERROR;
}

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
{WHITE} {

}
"\n" {
    curr_lineno += 1;
}
{INT_CONST} {
    cool_yylval.symbol = inttable.add_string(yytext, yyleng);
    return (INT_CONST);
}
{TRUE_CONST} {
    cool_yylval.boolean = 1;
    return (BOOL_CONST);
}
{FALSE_CONST} {
    cool_yylval.boolean = 0;
    return (BOOL_CONST);
}
{TYPEID} {
    cool_yylval.symbol = idtable.add_string(yytext, yyleng);
    return (TYPEID);
}
{OBJECTID} {
    cool_yylval.symbol = idtable.add_string(yytext, yyleng);
    return (OBJECTID);
}
<<EOF>> {
    return 0;
}
"{" {
    return '{';
}
"}" {
    return '}';
}
":" {
    return ':';
}
"(" {
    return '(';
}
")" {
    return ')';
}
";" {
    return ';';
}
"=" {
    return '=';
}
"<" {
    return '<';
}
"." {
    return '.';
}
"," {
    return ',';
}
"~" {
    return '~';
}
"-" {
    return '-';
}
"+" {
    return '+';
}
"*" {
    return '*';
}
"/" {
    return '/';
}
"@" {
    return '@';
}
. {
    cool_yylval.error_msg = yytext;
    return (ERROR);
}
%%
