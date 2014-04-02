%{
  #include "fas.h"
  #include "fas.tab.h"

  g_cell my_atoll ( const char* text );

%}

%x comment

DIGITS  [0-9]+
BLANK   [ \t\r\n]
BLANKS  {BLANK}+
WORD    [^ \t\r\n]+
LABEL   [^ \t\r\n:]+

%%

^\.inline     return DOT_INLINE;
^:imm         return COLON_IMMEDIATE;
^:            return COLON;

^{LABEL}:     {
                int len = strlen(yytext)-1;
                yylval.s = gc_malloc(len);
                memcpy(yylval.s,yytext,len);
                yylval.s[len] = '\0';
                return LABEL_CELL;
              }

^{LABEL}:char {
                int len = strlen(yytext)-5;
                yylval.s = gc_malloc(len);
                memcpy(yylval.s,yytext,len);
                yylval.s[len] = '\0';
                return LABEL_CHAR;
              }


^\(\(           BEGIN(comment);
<comment>^\)\)  BEGIN(INITIAL);
<comment>.      /* ignore */
<comment>\n     /* ignore */

\\.*$           /* comment */
"( "[^\)\n]*")"    /* comment */

\'.\'     {
            yylval.i = yytext[1];
            return DIGITS;
          }

-?{DIGITS}  {
            yylval.i = my_atoll(yytext);
            return DIGITS;
          }

{WORD}    {
            int len = strlen(yytext)+1;
            yylval.s = gc_malloc(len);
            memcpy(yylval.s,yytext,len);
            yylval.s[len] = '\0';
            return WORD;
          }

{BLANKS}            /* ignore */

%%

int yywrap() { return 1; }

g_cell my_atoll ( const char* text ) {
  g_cell result = 0;
  short sign = 0;
  if(*text == '-') {
    sign = 1;
    text++;
  }
  while(*text && *text >= '0' && *text <= '9') {
    result *= 10;
    result += *text - '0';
    text++;
  }
  if(*text) {
    fprintf(stderr,"Invalid remain: %s",text);
    exit(1);
  }
  return sign ? -result : result;
}