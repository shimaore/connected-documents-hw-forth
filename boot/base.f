\ base.f: Provides BASE and associated tools.

\ bootstrap.f only support decimal
VARIABLE BASE
: DECIMAL ( DECIMAL ) 10 BASE ! ;
: HEX     ( DECIMAL ) 16 BASE ! ;
: BIN                  2 BASE ! ;
\ Initialize BASE
DECIMAL

\ (BASE) : u -- u true | false
: (BASE) DUP BASE @ <  DUP INVERT IF NIP THEN ;

\ >DIGIT : c -- u true | false
: >DIGIT
  [CHAR] 0 -
  DUP 0<
  IF
    DROP FALSE
  ELSE
    DUP ( DECIMAL ) 10 <
    IF
      (BASE)
    ELSE
      DUP ( DECIMAL ) 9 <
      IF
        DROP FALSE
      ELSE
        ( DECIMAL ) 7 -
        (BASE)
      THEN
    THEN
  THEN ;

\ >NUMBER : u addr len -- u addr len
: >NUMBER
  BEGIN
    DUP 0> IF OVER C@ >DIGIT ELSE FALSE THEN
  WHILE
    UNROT >R >R >R
    BASE @ U* R> +
    R> CHAR+ R> 1-
  REPEAT ;

\ ?NUMBER : caddr -- n true | false
: ?NUMBER
  0 SWAP
  COUNT
  \ 0 addr len
  \ obtain sign
  OVER C@ [CHAR] - =
  \ 0 addr len -- flag
  IF  1- SWAP CHAR+ SWAP  -1 ELSE 1 THEN >R
  \ 0 addr len
  >NUMBER
  \ u addr len --
  NIP
  \ u len --
  \ apply sign
  SWAP R> +- SWAP
  \ remaining length should be 0
  DUP IF NIP THEN
  0=
  ;
