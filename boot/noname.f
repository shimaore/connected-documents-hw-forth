\ :NONAME creates an anonymous sub.

: :NONAME  ( -- xt )
  0 NFA !  \ no name, no NFA
  PFA,     \ save the location in PFA so that (e.g.) RECURSE works
  HERE     \ puts the xt on the stack
  ]        \ switch to compilation mode
  ;
