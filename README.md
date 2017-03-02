
# CHINE, a pretty compact opcode scheme

    +-+-----+----+
    |1|op:4 |op:3|  op:3 exuture before op:4
    +-+----------+
    |0|  op:7    |
    +-+----------+

    +-----+--------+----------+
    |Type | opcode | mnemonic |
    +-----+--------+----------+
    |     |	0      |  zbran.h |
    |     | 1      |  push.h  |
    |     |	2      |  dup     |
    | op3 |	3      |  rot     |
    |     |	4      |  over    |
    |     |	5      |  drop    |
    |     |	6      |  swap   |
    |     |	7      |  -       |
    +-----+--------+----------+
    |     |	8      |  +       |
    |     |	9      |  *       |
    |     |	10     |  =       |
    | op4 |	11     |  and     |
    |     |	12     |   or     |
    |     |	13     |  0<      |
    |     |	14     |  0=      |
    |     |	15     |  not     |
    +-----+--------+----------+
    |     | 16     |   /      |
    | op7 | 18     |   xor    |
    |     | 19     |          |
    |     | ...    |          |



## Pushing constants

|1| literal.h | const:4/signed |                 |
|0|                 literal.b  | const:8/signed  |
|0|                 literal.w  | const:16/signed |
|0|                 literal.l  | const:32/signed |

## Conditional branch when top of stack is zero

|1| zbran.h | offs:4/signed |                 |
|0|          zbran.b | offs:8/signed           |
|0|          zbran.w | offs:16/signed          |

## INSTRUCTIONS

| opname    |  stack effect       | comment       |
|-----------|---------------------|---------------|
| zbranch.h L:4 | ( f -- ) |
| push.h n:4 |	( -- n ) |
| dup        | ( a -- a a ) |
| rot       | ( a b c -- b c a ) | rotate down |
| over      | ( a b -- a b a ) |
| drop      | ( a -- ) |
| swap      | ( a b -- b a ) |
| -         | ( a b -- [ a - b ] ) |
| +         | ( x1 x2 -- [ x1+x2 ] )
| *         | ( x1 x2 -- [ x1*x2) )
| and       | ( a b -- [ a&b ] )
| or        | ( a b -- [ a|b ] )
| 0=        | ( a -- [ a==0 ] )
| 0<        | ( a -- [ a<0 ] )
| not       | ( a -- [ !a ] )
| /         | ( a b -- [ a/b ] ) |
| mod       | ( a b -- [ a%b ] ) |
| xor       | ( a b -- [ a^b ] )
| negate    | ( a -- [ -a ] ) |
| invert    | ( a -- [ ~a ] )  |
| lshift    | ( a u -- [ (uint)a << u ] ) |
| rshift    | ( a u -- [ (uint)a >> u ] ) |
| u<        | ( a b -- [ a<b ] ) |
| !         | ( a i -- ) | mem[i] = a |
| @         | ( i -- a ) | a = mem[i] |
| nop       | ( -- ) |
| u<=       | ( a b -- [ (uint)a <= (uint)b ] )
| ;         | ( -- ) R: ( addr -- )  |
| push.b n:16 | ( -- n )  |
| push.l n:32 | ( -- n )  |
| branch.b L:8 | ( -- ) |
| branch.w L:16 | ( -- ) |
| zbranch.w L:16 | ( f -- ) |
| ibranch.b u:8 L1:8 .. Ln:8 | ( i -- ) |
| ibranch.b u:16 L1:16 .. Ln:16 | ( i -- ) |
| call.b L:8 | ( -- ) R: ( -- c-addr ) |
| call.w L:16 | ( -- ) R: ( -- c-addr ) |
| sys.b u:8 |  ( x1 .. xn -- v f ) |
| exit | ( -- ) |
| yield | ( -- ) |

## compiler built-ins min,max,abs ...

	: = - 0= ;
	: <    - 0< ;
	: <=   - 1- 0< ;
	: >    swap - 0< ;
	: >=   swap - 1- 0< ;	
	: 1+ 1 + ;
	: 1- 1 - ;
	: mod over over / * - ;

	: min
      over over   ( a b -- a b a b )
      <         ( a b a b -- a b f )
      if
        drop         ( a b - a )
      else
        swap drop    ( a b - b )
      then ;

    : max ( a b -- [ max(a,b) ]
      over over   ( a b -- a b a b )
      <         ( a b a b -- a b f )
      if
        swap drop   ( a b - b )
	  else
        drop         ( a b - a )
      then ;

    : abs ( a -- [ |a| ] )
      dup 0<   ( a f )
	  if 
        negate
	  then ;

    : setbit    ( fld n -- [ fld or (1 << n) ] )
      1 swap lshift or ;

    : clrbit    ( fld n -- [ fld and ~(1 << n) ] )
      1 swap lshift invert and ;

    : togglebit ( fld n -- [ fld xor (1 << n) ] )
      1 swap lshift xor ;

    : tstbit   ( fld n -- [ fld and (1 << n) ] )
      1 swap lshift and ;

    : setclrbit ( fld n f -- [ if (f) setbit els clrbit ] )
	  if
	    setbit
	  else
	    clrbit
	  then ;
	  

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

| Name                | Code | Stack effect |
|---------------------|------|--------------|
| SYS\_PARAM\_FETCH   | 1    | ( i s -- v ) |
| SYS\_PARAM\_STORE   | 2    | ( v i s -- ) |
| SYS\_TIMER\_INIT    | 3    | ( i -- ) |
| SYS\_TIMER\_START   | 4    | ( i -- ) |
| SYS\_TIMER\_STOP    | 5    | ( i -- ) |
| SYS\_TIMER\_TIMEOUT | 6    | ( i -- ) |
| SYS\_TIMER\_RUNNING | 7    | ( i -- ) |
| SYS\_INPUT\_FETCH   | 8    | ( i k -- v ) |
| SYS\_SELECT\_TIMER  | 9    | ( i -- ) |
| SYS\_SELECT\_INPUT  | 10   | ( i -- ) |
| SYS\_DESELECT_ALL   | 11   | ( -- ) |
| SYS\_EMIT           | 12   | ( char -- ) |
| SYS\_KEY            | 13   | ( -- char ) |
| SYS\_QKEY           | 14   | ( -- f ) |
|
| SYS\_LED\_SET       | 12   | ( i -- ) |
| SYS\_LED\_CLR       | 13   | ( i -- ) |
| SYS\_LED\_MASK      | 14   | ( u16 -- ) |
| SYS\_SET\_LEVEL     | 15   | ( i u16 -- ) |
| SYS\_CAN\_LEVEL     | 16   | ( i u16 -- ) |

    gpio_output(pin)
    gpio_input(pin)
    gpio_set(pin)
    gpio_clr(pin)	

    digital_set(si)
    digital_clr(si)
    digital_mask(si)
    value = digital_get(si)
	
    analog_set(si, value)
    value = analog_get(si)

    can_send(si, MSG_ANALOG,  value)
    can_send(si, MSG_DIGITAL, value)
    can_send(si, MSG_ENCODER, value)

    uart_send(c)
    c = uart_recv()
