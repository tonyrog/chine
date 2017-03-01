
# COMPACT OPCODE SCHEME

    +-+-----+----+
    |1|op:4 |op:3|  op:3 exuture before op:4
    +-+----------+
    |0|  op:7    |
    +-+----------+

    +-----+--------+----------+
    |Type | opcode | mnemonic |
    +-----+--------+----------+
    |     |	0  |  zbran.h |
    |     |     1  |  push.h  |
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
    |     | 17     |   mod    |
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

| opcode| opname    |  stack effect | comment |
|-------|-----------|---------------|---------|
|   0   | zbranch.h L:4 | ( f -- ) |
|   1   | push.h    n:4 |	( -- n ) |
|   2   | dup       | ( a -- a a ) |
|	3   | rot       | ( a b c -- b c a ) | rotate down |
|	4   | over      | ( a b -- a b a ) |
|   5   | drop      | ( a -- ) |
|	6   | swap      | ( a b -- b a ) |
|	7   | -         | ( a b -- [ a - b ] ) |

|   8   | +         | ( x1 x2 -- [ x1+x2 ] )
|   9   | *         | ( x1 x2 -- [ x1*x2) )
|   11  | and       | ( a b -- [ a&b ] )
|   12  | or        | ( a b -- [ a|b ] )
|   13  | 0=        | ( a -- [ a==0 ] )
|   14  | 0<        | ( a -- [ a<0 ] )
|   15  | not       | ( a -- [ !a ] )

|   16  | /         | ( a b -- [ a/b ] ) |
|   17  | mod       | ( a b -- [ a%b ] ) |
|   18  | xor       | ( a b -- [ a^b ] )
|   19  | negate    | ( a -- [ -a ] ) |
|   20  | invert    | ( a -- [ ~a ] )  |
|   21  | lshift    | ( a n -- [ (uint)a << n] ) |
|   22  | rshift    | ( a n -- [ (uint)a >> n ] ) |
|   23  | arshift   | ( a n -- [ a >> n ] ) |
|   24  | u<        | ( a b -- [ a<b ] ) |
|   25  | !         | ( a i -- ) | mem[i] = a |
|   26  | @         | ( i -- a ) | a = mem[i] |
|   27  | nop       | ( -- ) |
|   28  | u<=       | ( a b -- [ (uint)a <= (uint)b ] )
|   29  | ;         | ( -- ) R: ( addr -- )  |
|   30  | push.b n:16 | ( -- n )  |
|   31  | push.l n:32 | ( -- n )  |
|   32  | branch.b L:8 | ( -- ) |
|   33  | branch.w L:16 | ( -- ) |
|   34  | zbranch.w L:16 | ( f -- ) |
|   35  | ibranch.b u:8 L1:8 .. Ln:8 | ( i -- ) |
|   36  | ibranch.b u:16 L1:16 .. Ln:16 | ( i -- ) |
|   37  | call.b L:8 | ( -- ) R: ( -- c-addr ) |
|   38  | call.w L:16 | ( -- ) R: ( -- c-addr ) |
|   39  | sys.b u:8 |  ( x1 .. xn -- v f ) |
|   40  | exit | ( -- ) |
|   41  | yield | ( -- ) |

## EXAMPLES of compact replacements

    -rot  = {rot,rot}
    2drop = {drop,drop}
    2dup  = {over,over}
    nip   = {swap,drop}
    tuck  = {swap,over}
    sqr   = {dup,*}
    2*    = {dup,+}
    <>    = {-,not}
    <>    = {=,not}
    >     = {swap,<}

## compiler built-ins min,max,abs ...

	: = - 0= ;
	: <    - 0< ;
	: <=   - 1- 0< ;
	: >    swap - 0< ;
	: >=   swap - 1- 0< ;	
	: 1+ 1 + ;
	: 1- 1 - ;

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
| SYS\_SELECT         | 9    | ( tmask imask -- ) |
| SYS\_EMIT           | 10   | ( char -- ) |
| SYS\_KEY            | 11   | ( -- char ) |

| SYS\_LED\_SET       | 12   | ( i -- ) |
| SYS\_LED\_CLR       | 13   | ( i -- ) |
| SYS\_LED\_MASK      | 14   | ( u16 -- ) |

| SYS\_SET\_LEVEL     | 15   | ( i u16 -- ) |

| SYS\_CAN\_LEVEL     | 16   | ( i u16 -- ) |


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
