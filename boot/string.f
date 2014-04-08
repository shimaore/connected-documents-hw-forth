: TYPE
  BEGIN
    ?DUP
  WHILE
    OVER C@ EMIT
    1- SWAP CHAR+ SWAP
  REPEAT
  DROP ;

\ XXX should error if WORD returns 0
: .(  [CHAR] ) WORD COUNT TYPE ; IMMEDIATE

: C"
  [CHAR] " WORD
  \ should error if 0
  WORD,
  POSTPONE LIT COMPILE,
  ; IMMEDIATE

: S" POSTPONE C" POSTPONE COUNT ; IMMEDIATE
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE

: CR     NEWLINE EMIT ;
: SPACE  BL EMIT ;

: SPACES  BEGIN DUP 0> WHILE 1- SPACE REPEAT DROP ;

: C,      HERE C! 1 ( CHARS ) CALLOT ;