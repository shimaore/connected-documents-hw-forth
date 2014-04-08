\ Number printing

: DIGIT>
  9 OVER < IF 7 + THEN
  [CHAR] 0 +
  ;

VARIABLE HLD
: <#    PAD HLD ! ;
: HOLD  -1 ( CHARS ) HLD +! HLD @ C! ;
: #>    DROP HLD @ PAD OVER - ;

: #     BASE @ U/MOD SWAP DIGIT> HOLD ;

: #S    BEGIN # DUP 0= UNTIL ;
: SIGN  0< IF [CHAR] - HOLD THEN ;
: U.    <# #S #> TYPE SPACE ;
: .     DUP >R ABS <# #S R> SIGN #> TYPE SPACE ;

