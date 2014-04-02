\ This is a "macro-assembler" source for myforth.
\ (c) 2005-2014, Stephane Alnet
\ ------------

BYE:
  start
interrupt_usart:
  usart

usart:
  usart_received
  \ substitute NEWLINE with 0
  DUP
  NEWLINE =
  0BRANCH usart.skip
  DROP LIT 0
usart.skip:
  \ store character at TIB + >TIB
  TIB @ >TIB @ + C!
  \ increment >TIB
  >TIB @ CHAR+ >TIB !
  \ return from interrupt (i.e. at the step following `sleep`)
  EXIT

EMIT:
  usart_send DROP
  EXIT

: TYPE ( addr len )
  ?DUP
  0BRANCH exit_DROP
  OVER C@ EMIT
  1- SWAP CHAR+ SWAP
  BRANCH TYPE

: DP      ( current dictionary pointer )
    LIT __DP      EXIT
__DP:
    end_of_dictionary

( There is no separate `char` memory. )
: CHAR-DP      ( current dictionary pointer )
    LIT __DP      EXIT

: STATE   ( current interpreter state: compiling if non-zero )
    LIT __STATE   EXIT
__STATE:
    0

: _LATEST ( DP value of last defined word )
    LIT __LATEST  EXIT
__LATEST:
    last_word
: PFA     ( PFA of word currently been defined )
    LIT __PFA     EXIT
__PFA:
    0
: NFA     ( NFA of word currently been defined )
    LIT __NFA     EXIT
__NFA:
    0
: TIB     ( The Input Buffer )
    LIT addr_tib EXIT
: >IN     ( Cursor offset in TIB )
    LIT __>IN     EXIT
__>IN:
    0
: >TIB     ( Cursor offset in TIB )
    LIT __>TIB     EXIT
__>TIB:
    0


\ ------------

\ Unavailable opcodes
: R>
  R@ RDROP EXIT
: 1+
  LIT 1 + EXIT
: FALSE
  LIT 0 EXIT
: TRUE
  FALSE 0= EXIT


((
  These are some very-easy-to-define words.

  : 2*     DUP + ;
  : 2DUP   OVER OVER ;
  : 2DROP  DROP DROP ;

  : ROT    >R SWAP R> SWAP ;
  : UNROT  SWAP >R SWAP R> ;
  : TUCK   SWAP OVER ;
  : NIP    SWAP DROP ;
  : +!     TUCK @ + SWAP ! ;
))

: 2*
    DUP + EXIT
: 10*
    2* DUP 2* 2* + EXIT
: 2DUP
    OVER OVER EXIT
: 2DROP
    DROP
exit_DROP:
    DROP EXIT

: ROT
    >R SWAP R> SWAP EXIT
: UNROT
    SWAP >R SWAP R> EXIT
: TUCK
    SWAP OVER EXIT
: NIP
    SWAP DROP EXIT
: +!
    TUCK @ + SWAP ! EXIT

: CHAR+
    1+ EXIT
: CELL+
    1+ 1+ 1+ EXIT

((
  DP is the address of the memory location containing the current
  dictionary pointer. HERE returns the value of the dictionary pointer.
  XXX: Should be redefined to use VOCABULARY, etc.

    : HERE DP @ ;
))

: HERE
    DP @ EXIT

: CHAR-HERE
    CHAR-DP @ EXIT

((
  : ALLOT      DP +! ;
  : CALLOT     CHAR-DP +! ;
  : ,          HERE !  1 CELLS ALLOT ;
  : COMPILE,   , ;
  : [LITERAL]  POSTPONE LIT , ;
  : LITERAL    [LITERAL] ; IMMEDIATE
))


: ALLOT   \ n --
    DP +! EXIT

: ,       \ n --
    HERE !  LIT 1
    BRANCH ALLOT

: CALLOT
    CHAR-DP +! EXIT

: COMPILE, \ xt --
    BRANCH ,

: [LITERAL] \ n --
    LIT LIT COMPILE, \ POSTPONE LIT
    BRANCH ,

:imm LITERAL
    [LITERAL] EXIT


((
  : -      NEGATE + ;
  : 1-     -1 + ;
  : 0<>    0= INVERT ;
  : 0<     neg_mask AND 0<> ;
  : <      - 0< ;
  : =      - 0= ;
  : <>     = INVERT ;
  : >      SWAP < ;
))

: INVERT
    TRUE XOR EXIT
: NEGATE
    INVERT 1+ EXIT

: -
    NEGATE + EXIT
: 1-
    LIT -1 + EXIT

: 0<>
    0= INVERT EXIT
: 0<
    LIT 8388608 AND ( 0x800000 )
    BRANCH 0<>
: <
    - 0< EXIT
: =
    - 0= EXIT
: <>
    = INVERT EXIT
: >
    SWAP < EXIT


((
  LATEST returns the NFA of the last defined word.
  [ puts the interpreter in evaluation mode.

  : LATEST _LATEST @ ;
  : [      0 STATE ! ; IMMEDIATE
))


: LATEST  \ -- laddr
    _LATEST @ EXIT
:imm [
    LIT 0 STATE ! EXIT


((
  QUIT is the main read-eval loop.
))


QUIT:
    [
    LIT 0 >TIB !
QUIT.begin: \ BEGIN
    sleep
    \ FIXME: this might make us miss characters from the USART
    >TIB @
    0BRANCH QUIT.begin
    TIB @ >TIB @ + C@
    0=
    0BRANCH QUIT.begin
    LIT 0 >IN !
    INTERPRET
    BRANCH QUIT.begin


((
  INTERPRET is the main interpreter.

  This version only works with unsigned binary numbers
  -- no BASE support, no sign support, no DOUBLE support --
  but is otherwise fully functional. It is used to
  compile the other bootstrap files.

  : INTERPRET
    BEGIN
      BL WORD
      ?DUP
    WHILE
      FIND
      ?DUP
      IF
        0< STATE @ AND
        IF COMPILE, ELSE EXECUTE THEN
      ELSE
        DUP ?UNUMBER
        IF
          NIP
          STATE @
          IF [COMPILE] LITERAL THEN
        THEN
      THEN
    REPEAT ;
))

INTERPRET:
    \ BEGIN
    BL WORD
    ?DUP
    0BRANCH exit_now \ WHILE
    FIND
    ?DUP
    0BRANCH INTERPRET.not_found \ IF
    0< STATE @ AND
    0BRANCH INTERPRET.execute \ IF
    COMPILE,
    BRANCH INTERPRET \ ELSE
INTERPRET.execute:
    EXECUTE
    BRANCH INTERPRET \ THEN
INTERPRET.not_found: \ ELSE
    DUP ?UNUMBER
    0BRANCH INTERPRET.error \ IF
    NIP STATE @
    0BRANCH INTERPRET \ IF
    LITERAL
    BRANCH INTERPRET \ THEN
INTERPRET.error:
    COUNT TYPE
    LIT '?' EMIT NEWLINE EMIT
    BYE

: NEWLINE
    LIT 10 EXIT


((
  \ No sign support, only base 10
  : ?UNUMBER
    0 SWAP
    COUNT
    >NUMBER
    NIP
    DUP IF NIP THEN
    0=
    ;
))

?UNUMBER: \ addr -- u true | false
    LIT 0 SWAP
    COUNT
    >NUMBER
    \ u addr len
    NIP
    \ u len
    DUP
    0BRANCH exit_0= \ IF
    NIP
exit_0=: \ THEN
    0=
    EXIT


((
  \ Only supports base 10
  : >NUMBER
    BEGIN
      DUP
      IF OVER C@ >DIGIT ELSE FALSE THEN
    WHILE
      UNROT >R >R >R
      10* R> +
      R> CHAR+ R> 1-
    REPEAT ;
))

>NUMBER:
    \ BEGIN : u addr len -- u addr len
    DUP
    0BRANCH exit_now \ IF
    OVER C@ >DIGIT
    0BRANCH exit_now \ WHILE
    \ -- u addr len digit
    UNROT
    \ -- u digit addr len
    >R >R >R         \ -- u  r: -- len addr digit
    10*
    R> +
    R> CHAR+
    R> 1-
    BRANCH >NUMBER \ REPEAT


((
  \ Only supports base 10
  : >DIGIT
    [CHAR] 0 -
    DUP 0< INVERT
    OVER 10 < AND
    DUP INVERT IF NIP THEN ;
))

>DIGIT: \ c -- u true | false
    LIT '0' -
    DUP 0< INVERT
    OVER LIT 10 <
    AND \ u flag --
    DUP INVERT
    0BRANCH >DIGIT.exit
    NIP
>DIGIT.exit:
    EXIT


((
  EXECUTE executes the code at the provided address.
  (The address should be a PFA.)

  : EXECUTE  >R ;
))

: EXECUTE  ( xt -- )
    >R
    EXIT


((
  Reads a delimited word from the input stream at the current position
  (as indicated by TIB+>IN) and returns it.

  : WORD
    TIB @ >IN @ + SWAP ENCLOSE
    >IN +!
    OVER - >R
    + 1-
    R@ IF R> OVER C! ELSE DROP R> THEN ;
))

: WORD  \ c "cXXXc" -- caddr | 0
    TIB @ >IN @ + SWAP ENCLOSE \ -- addr n1 n2 n3
    >IN +!
    OVER - \ -- addr n1 length
    >R     \ -- addr n1
    +
    R@     \ -- addr length
    0BRANCH WORD.exit \ IF
    HERE LIT 64 +     \ -- addr caddr
    R> OVER !     \ store the length first  -- addr caddr
    SWAP          \ -- caddr addr
    OVER CELL+ !  \ store the addr(c) second
    \ -- caddr
    EXIT
WORD.exit:
    DROP  \ --
    R>    \ -- 0
    EXIT


((
: ENCLOSE=
  >R
  BEGIN
    DUP C@
    DUP IF R@ = THEN
  WHILE CHAR+ REPEAT
  RDROP
  ;

: ENCLOSE<>
  >R
  BEGIN
    DUP C@
    DUP IF R@ <> THEN
  WHILE CHAR+ REPEAT
  RDROP ;

: ENCLOSE
  >R DUP
  DUP
  R@ ENCLOSE=
  2DUP SWAP - UNROT
  R@ ENCLOSE<>
  2DUP SWAP - UNROT
  R> ENCLOSE=
  SWAP -
  ;
))


: ENCLOSE  \ addr c -- addr n1 n2 n3
    >R DUP
    DUP
    R@ ENCLOSE=
    2DUP SWAP - UNROT
    R@ ENCLOSE<>
    2DUP SWAP - UNROT
    R> ENCLOSE=
    SWAP -
    EXIT

: ENCLOSE= \ addr1 c "cccX" -- addr2
    >R
ENCLOSE=.1: \ BEGIN
    DUP C@
    DUP
    0BRANCH ENCLOSE=.2 \ IF
    R@ =
ENCLOSE=.2: \ THEN
    0BRANCH exit_RDROP \ WHILE
    CHAR+
    BRANCH ENCLOSE=.1 \ REPEAT
exit_RDROP:
    RDROP
exit_now:
    EXIT


: ENCLOSE<> \ addr1 c "XXXc" -- addr2
    >R
ENCLOSE<>.1: \ BEGIN
    DUP C@
    DUP
    0BRANCH ENCLOSE<>.2 \ IF
    R@ <>
ENCLOSE<>.2: \ THEN
    0BRANCH exit_RDROP \ WHILE
    CHAR+
    BRANCH ENCLOSE<>.1 \ REPEAT


((
: FIND
  LATEST (FIND-NFA)
  DUP
  IF NIP DUP NFA>PFA SWAP ?IMMEDIATE THEN ;
))

: FIND  ( caddr -- immediate-xt 1 | xt -1 | caddr 0 )
    LATEST (FIND-NFA)       ( -- caddr nfa | caddr 0 )
    DUP
    0BRANCH exit_now \ IF
    NIP DUP NFA>PFA SWAP
    BRANCH ?IMMEDIATE


((
: NFA>FLAGS 2 + ;
: NFA>LFA   3 + ;
: NFA>PFA   NFA>LFA CELL+ ;
))

: NFA>FLAGS
    CELL+ CELL+ EXIT
: NFA>LFA \ nfa -- laddr
    NFA>FLAGS CELL+ EXIT
: NFA>PFA \ nfa -- xt
    NFA>LFA CELL+ EXIT


((
: (FIND-NFA)  ( caddr laddr -- caddr nfa | caddr 0 )
  BEGIN
    DUP
    IF 2DUP C= INVERT
    ELSE FALSE
    THEN
  WHILE
    NFA>LFA LFA@
  REPEAT
  ;
))

: (FIND-NFA) ( caddr laddr -- caddr nfa | caddr 0 )
    \ BEGIN
    DUP
    0BRANCH exit_now \ IF
    2DUP C= INVERT
    0BRANCH exit_now \ WHILE
    NFA>LFA @
    BRANCH (FIND-NFA) \ REPEAT


((
: ?IMMEDIATE ( nfa -- -1 (immediate) | 1 (non--immediate) )
  -1 SWAP
  NFA>FLAGS
  @ imm_set AND
  IF NEGATE THEN ;
))

: ?IMMEDIATE
    LIT -1 SWAP
    NFA>FLAGS
    @ LIT imm_set AND
    0BRANCH ?IMMEDIATE.exit \ IF
    NEGATE
?IMMEDIATE.exit:
    EXIT



((
: S=
  ROT
  OVER <>  \ must have the same length
  IF
    NIP 2DROP FALSE
  ELSE
    (S=)
  THEN ;

: (S=)  \ addr1(c) addr2(c) len -- bool
  BEGIN
    DUP >R
    IF 2DUP C@ SWAP C@ = ELSE FALSE THEN
  WHILE
    CHAR+ SWAP CHAR+
    R> 1-
  REPEAT
  2DROP R> 0=
;

))

: (S=)  \ addr1 addr2 len -- bool
    DUP >R
    0BRANCH (S=).exit \ end of string
    2DUP C@ SWAP C@ =
    0BRANCH (S=).exit \ WHILE
    CHAR+ SWAP CHAR+
    R> 1-
    BRANCH (S=) \ REPEAT
(S=).exit:      \ -- addr1 addr2   r: -- len
    2DROP R> 0=
    EXIT

: S=    \ addr1 len1 addr2 len2 -- bool
    ROT          \ addr1 addr2 len2 len1
    OVER <>      \ addr1 addr2 len2 bool
    0BRANCH (S=) \ IF
    NIP 2DROP LIT 0
    EXIT


\ : C= >R COUNT R> COUNT S= ;
: C=    \ caddr1 caddr2 -- bool
    >R COUNT R> COUNT
    BRANCH S=


((
  : BL 32 ;
  : 2@ DUP CELL+ @ SWAP @ ;
  : COUNT 2@ ;
  : ?DUP DUP IF DUP THEN ;
))

: BL
    LIT 32 EXIT

: 2@
    DUP CELL+ @ SWAP @ EXIT

: COUNT \ caddr -- addr(c) len
    BRANCH 2@

: ?DUP
    DUP
    0BRANCH ?DUP.exit \ IF
    DUP
?DUP.exit:  \ THEN
    EXIT


start: \ :NONAME
    QUIT
    BYE

((
  With all the words defined up to this point, we actually have a working
  interpreter that will start, accept input, recognize numbers and some
  words. The major drawback is that since the opcodes have not been
  defined as words in the dictionary, we will not be able to use them --
  which means we won't be able to do much.
  (As a sidenote, I would like to point out that there are actually all
  the required words to keep adding CHARs and CELLs to the dictionary, so
  in theory one could keep building the dictionary manually until the point
  where the following words are defined. I haven't tried.)

  To summarize:
  The following words deal with defining new words to simplify further
  extension of the dictionary, and specifically provide the ability to
  compile opcodes.f.
))

((
 : (FIND) BL WORD DUP IF FIND THEN ;
))

: (FIND)  ( "word" -- pfa 1 | pfa -1 | caddr 0 )
    BL WORD DUP
    0BRANCH exit_now \ IF
    BRANCH FIND


((
  This version of POSTPONE does not warn if the word does not exist.
  It is only defined here until a better definition is given in
  interpreter.f.
  : POSTPONE
    (FIND) 0<
    IF POSTPONE LITERAL POSTPONE COMPILE,
    ELSE COMPILE, THEN ; IMMEDIATE
))

:imm POSTPONE
    (FIND) 0<
    0BRANCH COMPILE,      \ IF
    LITERAL               \ POSTPONE LITERAL
    LIT COMPILE,          \ POSTPONE COMPILE,
    BRANCH COMPILE,


((

  CMOVE is required here in order to move the word from the TIB to the
  dictionary (HERE).

  : CMOVE
    BEGIN
      ?DUP
    WHILE
      1- >R
      2DUP SWAP C@ SWAP !
      CHAR+ SWAP CHAR+ SWAP
      R>
    REPEAT
    2DROP ;


))

: CMOVE ( src dst count -- )
    \ BEGIN
    ?DUP
    0BRANCH 2DROP \ WHILE
    1- >R
    2DUP SWAP C@ SWAP C!
    CHAR+ SWAP CHAR+ SWAP
    R>
    BRANCH CMOVE \ REPEAT

((

   Dictionary structure:

   ' WORD, ' will append a name to the dictionary.
   The word is stored in the CHAR memory.
   In the cell memory we store the length and the char-addr of the
   string. The following cell is used to store the word flag(s).

   Following the name is the pointer that links to the previously
   defined name. (Dictionary entries are thus in a linked list.)
   This field is called the LFA.
   Note:  :NONAME words do not have a name nor an LFA since they
          do not belong to the dictionary.

   : LFA,     ALIGN LATEST , ;

   There is no CFA since all words are executable,
   and in the virtual machine, opcodes are differentiated by value.
   (So there is no need for a CFA to differentiate sub-calls vs opcodes.)

   Therefore, immediately following the LFA is the PFA.

   : PFA,     ALIGN HERE PFA !  ;

   The first CELL of the PFA is always an instruction (either an opcode or
   a sub-call).
))

: WORD, ( caddr -- )
    COUNT           \                  -- addr len
    DUP >R DUP ,    \ compile length   -- addr len
    CHAR-HERE DUP , \ compile addr(c)  -- addr len char-here
    LIT 0 ,         \ compile flags
    \ -- addr len char-here
    SWAP CMOVE
    R> CALLOT
    EXIT

: LFA,
    LATEST , EXIT
: PFA,
    HERE PFA ! EXIT


((

  LATEST! is called at the end of a definition in order to update the
  head of the linked-list that contains the names of the words.
  (That linked-list is what we call the dictionary.)
  Note: Words defined by :NONAME do not belong to the dictionary (they
  are anonymous). In the case of a word defined by :NONAME, NFA will
  contain 0, and LATEST! will not update the head of the list.

  : LATEST!  NFA @ ?DUP IF _LATEST ! THEN ;

))

: LATEST!
    NFA @ ?DUP
    0BRANCH LATEST!.exit
    _LATEST !
LATEST!.exit:
    EXIT

((

  HEADER creates a complete named definition starter for a word,
  including name, LFA, and PFA.

  : HEADER
    HERE NFA !
    BL WORD
    \ should error if 0
    WORD,
    LFA,
    PFA,
    ;

))

: HEADER
    HERE NFA !
    BL WORD
    WORD, LFA, PFA, EXIT

((

  Switches from evaluation mode to compilation mode.

  : ] imm_set STATE ! ;

))

: ]
    LIT imm_set STATE ! EXIT

((

  : starts the definition of a new word that is meant to be executed later.

  : : HEADER ] ;

))

: :
    HEADER ] EXIT

((

   ; terminates a definition started by : or :NONAME, and switches back
   to evaluation mode.

   : ;
     POSTPONE EXIT
     LATEST!
     [COMPILE] [
     ALIGN
     0 PFA !       \ clear the PFA pointer
     ; IMMEDIATE

))

:imm ;
    LIT EXIT COMPILE,   \ POSTPONE EXIT
    LATEST!
    [
    LIT 0 PFA !
    EXIT

((

  IMMEDIATE marks the last word as immediate.

  : IMMEDIATE LATEST NFA>FLAGS DUP @ imm_set OR SWAP ! ;

))

: IMMEDIATE
    LATEST NFA>FLAGS DUP @ LIT imm_set OR SWAP ! EXIT


((

  The CREATE .. DOES> couple.

  The executable part of the PFA of CREATE only uses one CELL, which is
  immediately followed by the data area of the word.

  CREATE
  If the word is simply created by CREATE but never completed by DOES>,
  then the PFA cell contains a call to (CREATE), which will pop its return
  address (=the address of the CELL following the PFA) onto the data stack,
  and return the call to the calling word, in effect putting the address
  of the data area on the data stack.

  A word created by CREATE looks like:

    PFA:
         (CREATE)    \ compiled by CREATE
         <data>      \ location of >BODY

  CREATE .. DOES>
  If the word is created by CREATE and then completed by DOES>, DOES>
  will compile (DOES>) and then R> in the creating word. (DOES>) is the
  last word executed after CREATE, and it will update the PFA of the word
  that has just been created with the address that follows (DOES>) in the
  creating word. In other words, the PFA of the word created by CREATE
  will now contain a call to the word that follows (DOES>), which is R>
  (and then the words following R> in the original definition of the creating
  word). At the time the virtual machine encounters R>, the return stack
  will contain the address following the PFA, and R> will put that address
  onto the data stack. Then the execution continues in the creating word.

  In other words, the creating word is rewritten as:

        CREATE
        ...
        (DOES>)   \ DOES>  part 1
    next:
        R>        \ DOES>  part 2
        ...
        ;

  A word created by CREATE .. DOES> looks like:

    PFA:
        <next:>   \ as above, compiled by (DOES>)
        <data>    \ location of >BODY


  >BODY gives the data area of a word created by CREATE based on its PFA.
  Since the PFA is only one cell long in all cases, the data area starts
  at the first CELL after the PFA.

  \ (CREATE) is the default PFA for a word created by CREATE
  : (CREATE) R> ;

  : CREATE
    HEADER
    POSTPONE (CREATE)  \ default PFA
    LATEST!
    ;

  \ (DOES>) replaces the default PFA
  : (DOES>)
    R> PFA @ !         \ replace the default PFA
    0 PFA !            \ clear the PFA pointer
    ;

  : DOES>
    POSTPONE (DOES>)   \ last instruction of the defining word
    POSTPONE R>        \ first instruction of the defined word
    ; IMMEDIATE

  \ >BODY
  \ returns a pointer to the data section of a word created by CREATE
  : >BODY  CELL+ ;


  Note: DOES> and >BODY are not actually defined here since we don't
        need them right away. See lib/does.f.

))

: (CREATE)
    R> EXIT

: (DOES>)
    R> PFA @ !
    LIT 0 PFA !
    EXIT

: CREATE
    HEADER
    LIT (CREATE) COMPILE,  \ POSTPONE (CREATE)
    LATEST!
    EXIT


((

  :opcode is used to register the opcodes.

  A straightforward way to define the opcodes would be to simply create
  words that contain (1) the actual opcode and (2) the opcode for EXIT.
  However this doesn't work well because this results in a lot of subcalls,
  when the opcodes were created to be used natively (their space overlaps
  with the memory space). Also EXIT cannot be defined that way.

  Instead, :opcode defines opcode words that will contain:
  (a) an executable version of the opcode, to be used interactively;
      that version uses the "opcode + EXIT" schema since timing is less a
      constraint when used interactively;
  (b) a compilation version that will COMPILE, the opcode in the current
      definition.

  :opcode stores the opcode value as the first CELL of the data section
  ('BODY') provided by CREATE, and follows it by an explicit EXIT.
  In the DOES> section, the BODY is used to either
  (a) EXECUTE the opcode and the EXIT, or
  (b) retrieve the value of the opcode and add it to the definition of
      the word being compiled.

  : :opcode CREATE COMPILE, POSTPONE EXIT IMMEDIATE
            DOES> STATE @ IF @ COMPILE, ELSE EXECUTE THEN ;

))

last_word:

: :opcode  ( n 'name' --- )
    CREATE
    COMPILE,
    LIT EXIT COMPILE,   \ POSTPONE EXIT
    IMMEDIATE
    (DOES>)   \ DOES>
opcode.does:
    R>        \ DOES>
    STATE @
    0BRANCH EXECUTE
    @ COMPILE,
    EXIT

((
  The words created by :opcode will therefor look like this:

  for example for    n_DUP :opcode DUP

  :imm DUP
    opcode.does \ installed by DOES>
    n_DUP
    EXIT
    (DOES>)

))

((

  At this point we have a complete base system that can compile opcodes.
  We need one last definition to simplify the building of binary images.

  Note: last_word is used to initialize LATEST (see top of this file).

))

((

  end_of_dictionary marks the end of the dictionary in this bootstrap
  file, and is used to initialize DP (see top of this file).

))

end_of_dictionary:
