\ +C: u1 u2 -- sum carry
: +C
  2DUP + >R
  \ If the result is lower than either operands then overflow happened.
  R@ U>
  SWAP
  R@ U>
  OR
  IF 1 ELSE 0 THEN
  R> SWAP
;

\ U/MOD : u1 u2 -- rem quot
: U/MOD
  1 >R >R 0
  \ R: quot u2 --
  \ S: rem-l(u1) rem --
  BEGIN
    SWAP DUP +C
    ROT 2* OR
    R@ - DUP 0<
    IF R@ + 0 ELSE 1 THEN
    R> SWAP R>
    DUP +C >R OR
    R> SWAP >R SWAP >R
  UNTIL
  NIP RDROP R>
  ;

: U/    U/MOD NIP ;
: UMOD  U/MOD DROP ;

