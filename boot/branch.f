
\ (BRANCH,LIT) MARK .. FORWARD (absolute)
: MARK     HERE 0 , ;
: FORWARD  HERE SWAP ! ;

\ HERE .. (BRANCH,LIT) BACK (absolute)
: BACK   , ;

\ AHEAD (not executed) .. THEN (continues here)
\ (i.e. skip/jump ahead)
: AHEAD  POSTPONE BRANCH MARK ; IMMEDIATE

: IF     POSTPONE 0BRANCH MARK ; IMMEDIATE
: THEN   FORWARD ; IMMEDIATE
: ELSE   >R POSTPONE AHEAD R> POSTPONE THEN ; IMMEDIATE

: BEGIN  HERE ; IMMEDIATE
: AGAIN  POSTPONE BRANCH BACK ; IMMEDIATE
: UNTIL  POSTPONE 0BRANCH BACK ; IMMEDIATE
: WHILE  >R POSTPONE IF R> ; IMMEDIATE
: REPEAT POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE

: RECURSE POSTPONE BRANCH PFA @ COMPILE, ; IMMEDIATE