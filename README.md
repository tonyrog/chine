
# CHINE, a pretty compact opcode scheme

Chine is a byte code machine. The chine
machine and its byte code is easy to port to a number 
of architectures.

## INSTRUCTIONS op3/op6

| opname    |  stack effect        | comment       |
|-----------|----------------------|---------------|
| dup       | ( a -- a a )         |
| rot       | ( a b c -- b c a )   | rotate down |
| over      | ( a b -- a b a )     |
| drop      | ( a -- )             |
| swap      | ( a b -- b a )       |
| -         | ( a b -- [ a - b ] ) |
| +         | ( x1 x2 -- [ x1+x2 ] )
| *         | ( x1 x2 -- [ x1*x2) )
| nop       | ( -- )               |
| and       | ( a b -- [ a&b ] )
| or        | ( a b -- [ a|b ] )
| xor       | ( a b -- [ a^b ] )
| 0=        | ( a -- [ a==0 ] )
| 0<        | ( a -- [ a<0 ] )
| not       | ( a -- [ !a ] )
| invert    | ( a -- [ ~a ] )  |
| negate    | ( a -- [ -a ] ) |
| /         | ( a b -- [ a/b ] ) |
| shift     | ( a n -- [ (uint)a << n OR (int)a >> -n ] ) |
| !         | ( a i -- ) | mem[i] = a |
| @         | ( i -- a ) | a = mem[i] |
| >r        | ( n -- ) R: ( -- n )
| r>        | R: ( n -- ) ( -- n )
| r@        | R: ( n -- n ) ( -- n )
| sys u:8   |    ( x1 .. xn -- v f ) |
| exit      | ;  ( -- ) R: ( addr -- )  |
| yield     |    ( -- )  |
| []        | ( caddr i -- n )
| execute   | ( caddr -- )

## INSTRUCTIONS jop

| opname    |  stack effect       | comment            |
|-----------|---------------------|--------------------|
| jmpz      | ( f -- )            |  top==0            |
| jmpnz     | ( f -- )            |  top!=0            |
| next      | ( -- )              |  rtop>=0           |
| jmplz     | ( f -- )            |  top<0             |
| jmp       | (  -- )             |                    |
| call      | ( -- )              |                    |
| literal   | ( -- n )            |                    |
| array     | ( -- caddr )        |                    |


## compiler built-ins min,max,abs ...

	: 1+   1 + ;
	: 1-   1 - ;
	: lshift shift ;
	: rshift negate shift ;
	: <    - 0< ;
	: >    swap - 0< ;
	: <=   - 0<= ;
	: >=   swap - 0<= ;
	: =    - 0= ;
	: 2dup over over ;
	: 2drop drop drop ;
        : u< 2dup xor 0< 
	  if swap drop 0< else - 0< then ;
        : u<= 2dup xor 0< 
	  if swap drop 0< else - 0<= then ;
	: u> swap u< ;
        : u>= swap u<= ;
	: 0<> 0= not ;
	: 0>  0 > ;
	: 0<= 1- 0< ;
        : abs ( a -- [ |a| ] )
          dup 0< if negate then ;
	: min ( a b -- [ min(a,b) ] )
          2dup < if drop else swap drop then ;
        : max ( a b -- [ max(a,b) ]
          2dup < if swap drop else drop then ;
        : nip swap drop ;
	: tuck swap over ;
	: -rot rot rot ;
	: arshift dup 32 swap -
		  -1 swap shift -rot negate shift or ;
	: 2* dup + ;
	: 2/ 1 arshift ;
	: sqr dup * ;
	: mod 2dup / * - ;

	: jmp* >r exit ;
	: SEMI exit ;
	: setbit    ( fld n -- [ fld or (1 << n) ] )
	  1 swap lshift or ;
	: clrbit    ( fld n -- [ fld and ~(1 << n) ] )
	  1 swap lshift invert and ;
	: togglebit ( fld n -- [ fld xor (1 << n) ] )
          1 swap lshift xor ;
	: tstbit   ( fld n -- [ fld and (1 << n) ] )
          1 swap lshift and ;
	: setclrbit ( fld n f -- [ if (f) setbit els clrbit ] )
	  if setbit else clrbit then ;

	: jmp* ( caddr -- ) >r exit ;
	  
## non branch alternatives ( mostly for fun )

    : min ( a b -- [ min(a,b) ]
      over over over over ( a b -- a b a b a b )
      - 0< ( a b a b [ sign(a - b) ] )
      -rot ( a b [ sign(a - b) ] a b )
      xor  ( a b [ sign(a - b) ] [ a xor b ] )
	  and  ( a b [ sign(a - b) and (a xor b) ] )
	  xor  ( a [ b xor ( sign(a-b) and (a xor b) ) ] )
	  swap drop ; ( [ b xor ( sign(a-b) and (a xor b) ) ] )

    : abs ( a -- [ |a| ] )
      dup 0<     ( a f )
      dup        ( a f f )
      rot swap   ( f a f )
      negate xor ( f [ a xor -f ] )
      +          ( [ f + (a xor -f) ] )
    ;

    : setclrbit ( w n f -- [ if (f) w |= m; else w &= ~m end ] )
      negate         ( w n -f )
      swap 1 swap lshift ( w -f m )
      rot            ( -f m w )
      rot            ( m w -f )
      over           ( m w -f w )
      xor            ( m w [-f xor w] )
      rot            ( w [-f xor w] m )
      and            ( w [ ((-f xor w) and m ) ] )
      xor            ( [ w xor ((-f xor w) and m ) ] )
    ;

## System calls

| Name                | Code | Stack effect|comment|
|---------------------|------|-------------|-------|
| param@         | ( i s -- v )     |       |
| param!         | ( i si v -- )    |       |
| timer_init     | ( i -- )         |       |
| timer_start    | ( i -- )         |       |
| timer_stop     | ( i -- )         |       |
| timer_timeout  | ( i -- )         |       |
| timer_running  | ( i -- )         |       |
| input@         | ( i k -- v )     |       |
| select_timer   | ( i -- )         |       |
| deselect_timer | ( i -- )         |       |
| select_input   | ( i -- )         |       |
| dselect_input  | ( i -- )         |       |
| deselect_all   | ( -- )           |       |
| uart_send      | ( c -- )      |       |
| uart_recv      | ( -- c )      |       |
| uart_available | ( -- f )         |       |
| timer_now      | ( -- u )         | ms sinc system start |
| gpio_input     | ( i -- )         |       |
| gpio_output    | ( i -- )         |       |
| gpio_set       | ( i -- )         |       |
| gpio_clr       | ( i -- )         |       |
| gpio_get       | ( i -- n )       |       |
| analog_send    | ( i u16 -- )     |       |
| analog_recv    | ( i -- u16 )     |       |
| can_send       | ( u16 si n -- )  |       |
