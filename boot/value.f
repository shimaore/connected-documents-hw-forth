\ VALUE has the same definition as CONSTANT
: VALUE CREATE , DOES> @ ;
: TO ' >BODY ! ;

\ FUNCTION executes instead of retrieving
: FUNCTION CREATE 0 , DOES> @ ?EXECUTE ;
\ use TO to modify the function:   xt TO function-name
