: RESOLVE-FORWARD
  BEGIN
    ?DUP
  WHILE
    DUP @ SWAP FORWARD
  REPEAT
  ;

( case-sys :: endof-here )
: CASE ( C: -- case-sys ) ( -- )
  0 ; IMMEDIATE

: (OF) ( x1 x2 -- ftrue | x1 ffalse )
  OVER =
  DUP IF NIP THEN
  ;

( of-sys :: here )
: OF ( C: -- of-sys ) ( x1 x2 -- | x1 )
  POSTPONE (OF)
  POSTPONE IF
  ; IMMEDIATE

: ENDOF  ( C: case-sys1 of-sys -- case-sys2 )
  SWAP
  ( S: of-here endof-here -- )
  POSTPONE BRANCH
  HERE >R
  , ( Store the last endof-pointer in the linked list )
  POSTPONE THEN
  R> ( -- case-sys )
  ; IMMEDIATE

: ENDCASE ( C: case-sys -- ) ( x -- )
  POSTPONE DROP
  RESOLVE-FORWARD
  ; IMMEDIATE
