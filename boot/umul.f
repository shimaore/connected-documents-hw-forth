: U*
  2DUP > IF SWAP THEN
  >R
  0 SWAP
  BEGIN ?DUP WHILE
    DUP 1 AND
    IF SWAP R@ + SWAP THEN
    R> 2* >R
    2/
  REPEAT
  RDROP ;
