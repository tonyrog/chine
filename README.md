
# CHINE, a pretty compact opcode scheme

    |000| op5  |

    |01|000|jop| <L:8>   jmpx, call, literal
    |01|001|jop| <L:16>  jmpx, call, literal
    |01|011|jop| <L:32>  jmpx, call, literal

    |10|LLL|jop| jmpx, call, literal

    |11|op3|op3|  two packed opcode first 8 regular opcodes

## INSTRUCTIONS aop


| opname    |  stack effect       | comment            |
|-----------|---------------------|--------------------|
| jmpz      | ( f -- )            |  top==0            |
| jmpnz     | ( f -- )            |  top!=0            |
| jmpgtz    | ( f -- )            |  top>0             |
| jmpgez    | ( f -- )            |  top>=0            |
| jmp       | (  -- )             |                    |
| call      | (  -- )             |                    |
| literal   | ( -- n )            |                    |
| jmpi      | ( i --  )           |                    |

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
| and       | ( a b -- [ a&b ] )
| or        | ( a b -- [ a|b ] )
| 0=        | ( a -- [ a==0 ] )
| 0<        | ( a -- [ a<0 ] )
| 0<=       | ( a -- [ a<=0 ] )
| not       | ( a -- [ !a ] )
| /         | ( a b -- [ a/b ] ) |
| xor       | ( a b -- [ a^b ] )
| negate    | ( a -- [ -a ] ) |
| invert    | ( a -- [ ~a ] )  |
| lshift    | ( a u -- [ (uint)a << u ] ) |
| rshift    | ( a u -- [ (uint)a >> u ] ) |
| u<        | ( a b -- [ a<b ] ) |
| u<=       | ( a b -- [ (uint)a <= (uint)b ] )
| !         | ( a i -- ) | mem[i] = a |
| @         | ( i -- a ) | a = mem[i] |
| nop       | ( -- ) |
| ret       | ( -- ) R: ( addr -- )  |
| sys.b u:8 |  ( x1 .. xn -- v f ) |
| exit      | ( -- )  |
| yield     | ( -- )  |

## compiler built-ins min,max,abs ...

	: =    - 0= ;
	: <    - 0< ;
	: <=   - 0<= ;
	: >    swap - 0< ;
	: >=   swap - 0<= ;
	: 1+   1 + ;
	: 1-   1 - ;
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

| Name                | Code | Stack effect |    comment   |
|---------------------|------|--------------|--------------|
| SYS\_PARAM\_FETCH   | 1    | ( i s -- v ) |  param@      |
| SYS\_PARAM\_STORE   | 2    | ( v i s -- ) |  param!      |
| SYS\_TIMER\_INIT    | 3    | ( i -- )     |  timer_init  |
| SYS\_TIMER\_START   | 4    | ( i -- )     |  timer_start |
| SYS\_TIMER\_STOP    | 5    | ( i -- )     |  timer_stop  |
| SYS\_TIMER\_TIMEOUT | 6    | ( i -- )     |  timer_timeout |
| SYS\_TIMER\_RUNNING | 7    | ( i -- )     |  timer_running |
| SYS\_INPUT\_FETCH   | 8    | ( i k -- v ) |  input@        |
| SYS\_SELECT\_TIMER  | 9    | ( i -- )     |  select_timer |
| SYS\_SELECT\_INPUT  | 10   | ( i -- )     |  select_input |
| SYS\_DESELECT_ALL   | 11   | ( -- )       |  deselect_all |
| SYS\_EMIT           | 12   | ( char -- )  |  uart_send(c) |
| SYS\_KEY            | 13   | ( -- char )  |  c=uar_recv() |
| SYS\_QKEY           | 14   | ( -- f )     |  available()   |
| SYS\_DIG\_SET       | 15   | ( i -- )     |  digital_set(si)   |
| SYS\_DIG\_CLR       | 16   | ( i -- )     |  digital_clr(si)   |
| SYS\_DIG\_MASK      | 17   | ( u16 -- )   |  digital_mask(mask) |
| SYS\_SET\_LEVEL     | 18   | ( i u16 -- ) |  analog_set(si,level) |
| SYS\_CAN\_LEVEL     | 19   | ( i u16 -- ) |  can_send(si,MSG_ANALOG,level)

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
