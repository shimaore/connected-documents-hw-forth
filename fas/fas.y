%{
  #include "fas.h"
  #include "main.h"
%}

%union {
	g_cell  i;
	char*   s;
}

%token DOT_INLINE
%token COLON COLON_IMMEDIATE
%token DIGITS WORD LABEL_CELL LABEL_CHAR

%start  contents

%%

contents:
  content
  | contents content
  ;

content:
    DOT_INLINE WORD value { create_name($<s>2,$<i>3); }
  | COLON WORD            { compile_header($<s>2,0); }
  | COLON_IMMEDIATE WORD  { compile_header($<s>2,imm_set); }
  | LABEL_CELL            { create_name_here($<s>1); }
  | LABEL_CHAR            { create_name_here($<s>1); }
  | DIGITS                { compile_value($<i>1); }
  | WORD                  { compile_word($<s>1); }
  ;

value:
    DIGITS { $<i>$ = $<i>1; }
  | WORD   { $<i>$ = get_value($<s>1); }

%%
