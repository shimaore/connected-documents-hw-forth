: DOES>
  POSTPONE (DOES>)   \ last instruction of the defining word
  POSTPONE R>        \ first instruction of the defined word
  ; IMMEDIATE

\ >BODY
\ returns a pointer to the data section of a word created by CREATE
: >BODY  ( xt -- data-addr )  CELL+ ;
