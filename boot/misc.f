
: 0  [  ( DECIMAL )  0 ] LITERAL ;
: 1  [  ( DECIMAL )  1 ] LITERAL ;
: 2  [  ( DECIMAL )  2 ] LITERAL ;
: 3  [  ( DECIMAL )  3 ] LITERAL ;

: -1     [ 1 NEGATE ] LITERAL ;
: FALSE  [ 0 DUP XOR ] LITERAL ;
: TRUE   [ FALSE INVERT ] LITERAL ;

: U>     SWAP U< ;
: <=     > INVERT ;
: >=     < INVERT ;
: 0>     0 > ;

: +-   0< IF NEGATE THEN ;
: ABS  DUP +- ;

: CONSTANT  CREATE , DOES> @ ;
: VARIABLE  CREATE 0 , DOES> ;

: '    (FIND) DROP ;
: [']  ' [LITERAL] ; IMMEDIATE

: ?EXECUTE ?DUP IF EXECUTE THEN ;
