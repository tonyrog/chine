# CHINE, a pretty compact opcode scheme

Chine is a byte code machine. The chine
machine and its byte code is easy to port to a number 
of architectures.

## INSTRUCTIONS op3/op6

stack effect diagram works like before -- after
the right most element is top of the stack
so in rot ( a b c -- b c a ) then c is top of the
stack before rot and a is top of stack after rot


| opname    |  stack effect        | comment       |
|-----------|----------------------|---------------|
| dup       | ( a -- a a )         |
| rot       | ( a b c -- b c a )   | rotate down |
| over      | ( a b -- a b a )     |
| drop      | ( a -- )             |
| swap      | ( a b -- b a )         |
| -         | ( a b -- [ a-b ] )     |
| +         | ( x1 x2 -- [ x1+x2 ] ) |
| *         | ( x1 x2 -- [ x1*x2 ] ) |
| nop       | ( -- )                 |
| and       | ( a b -- [ a&b ] )
| or        | ( a b -- [ a\|b ] )
| xor       | ( a b -- [ a^b ] )
| 0=        | ( a -- [ a==0 ] ) | true = -1
| 0<        | ( a -- [ a<0 ] )  | true = -1
| not       | ( a -- [ ~a ] )
| negate    | ( a -- [ -a ] ) |
| /         | ( a b -- [ a/b ] ) |
| shift     | ( a n -- [ (uint)a << n OR (int)a >> -n ] ) |
| !         | ( n i -- ) | mem[i] = n |
| @         | ( i -- n ) | n = mem[i] |
| >r        | ( n -- ) R: ( -- n )
| r>        | R: ( n -- ) ( -- n )
| r@        | R: ( n -- n ) ( -- n )
| sys u:8   |    ( x1 .. xn -- v f ) |
| exit      | ;  ( -- ) R: ( addr -- )  |
| yield     |    ( -- )  |
| []        | ( caddr i -- n ) | data access via index
| execute   | ( caddr -- ) | 
| fp@       | ( -- fp )    | fetch frame pointer
| fp!       | ( fp -- )    | set frame pointer
| sp@       | ( -- sp )    | fetch stack pointer
| sp!       | ( sp -- )    | set stack pointer
| c!        | ( n i -- )   | ((byte*)mem)[i] = n |
| c@        | ( i -- n )   | n = ((byte*)mem)[i] |
| size      | ( caddr -- n ) | number of array elements |

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
| arg <i>   | ( -- a )            | a = frame[i]       |
| array <n> | ( -- caddr )        |                    |
| fenter    | ( a b c -- )        | set new fp         |
| fleave    | ( a b c -- a b c )  | restore old frame  |
| fset <i>  | ( a -- )            | frame[i]=a         |

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

Stack effects are written as ( before -- after ) where
stacks are describes as " last ... 2nd top "  that is
top is to the right, the order it is typed in.

| Name            | Stack effect     |Comment|
|-----------------|------------------|-------|
| now             | ( -- u )         | ms sinc system start |
| emit            | ( c -- )         |       |
| recv            | ( -- c )         |       |
| avail           | ( -- f )         |       |
| param@          | ( i si -- v )    |       |
| param!          | ( i si v -- )    |       |
| timer\_init     | ( i -- )         |       |
| timer\_start    | ( i -- )         |       |
| timer\_stop     | ( i -- )         |       |
| timer\_timeout  | ( i -- )         |       |
| timer\_running  | ( i -- flag )    |       |
| input@          | ( i k -- v )     |       |
| select\_timer   | ( i -- )         |       |
| deselect\_timer | ( i -- )         |       |
| select\_input   | ( i -- )         |       |
| dselect\_input  | ( i -- )         |       |
| deselect\_all   | ( -- )           |       |
| gpio\_input     | ( i -- )         |       |
| gpio\_output    | ( i -- )         |       |
| gpio\_set       | ( i -- )         |       |
| gpio\_clr       | ( i -- )         |       |
| gpio\_get       | ( i -- n )       |       |
| analog\_send    | ( i u16 -- )     |       |
| analog\_recv    | ( i -- u16 )     |       |
| uart\_connect   | ( baud mode tty -- fd t | err f ) | |
| uart\_send      | ( fd u8 -- t | err f ) | |
| uart\_recv      | ( fd -- u8 t | err f ) | |
| uart\_avail     | ( fd -- flag ) | |
| can\_disconnect | ( fd -- t | err f ) | |
| can\_connect    | ( bitrate mode dev -- fd t | err f )
| can\_send       | ( fd fid len A B -- t | err f )
| can\_recv       | ( fd -- A B len fid t | 0 fid t | err f )
| can\_avail      | ( fd -- flag )
| can\_disconnect | ( fd -- t | err f )
| file\_open      | ( name flags -- fd t | fd f ) | |
| file\_write     | ( fd buf n -- n t | err f ) | |
| file\_read      | ( fd buf n -- n t | err f ) | |
| file\_close     | ( fd -- t | err f ) | |
| file\_seek      | ( fd offset whence -- offs t | err f ) | |

# Compact instructions

    [dup,dup] : ( [a] -- [a,a,a] )
    [dup,rot] : ( [a,b] -- [b,b,a] )
    [dup,over] ( multiple ) ( [a] -- [a,a,a] )
    [dup,drop] = id
    [dup,swap] : ( [a] -- [a,a] )
    [dup,'-'] : ( [a] -- [{const,0}] )
    [dup,'+'] : ( [a] -- [{'+',a,a}] )
    [dup,'*'] : ( [a] -- [{'*',a,a}] )
    [rot,dup] : ( [a,b,c] -- [b,c,a,a] )
    [rot,rot] : ( [a,b,c] -- [c,a,b] )
    [rot,over] : ( [a,b,c] -- [b,c,a,c] )
    [rot,drop] : ( [a,b,c] -- [b,c] )
    [rot,swap] : ( [a,b,c] -- [b,a,c] )
    [rot,'-'] : ( [a,b,c] -- [b,{'-',c,a}] )
    [rot,'+'] : ( [a,b,c] -- [b,{'+',c,a}] )
    [rot,'*'] : ( [a,b,c] -- [b,{'*',c,a}] )
    [over,dup] : ( [a,b] -- [a,b,a,a] )
    [over,rot] : ( [a,b] -- [b,a,a] )
    [over,over] : ( [a,b] -- [a,b,a,b] )
    [over,drop] = id
    [over,swap] : ( [a,b] -- [a,a,b] )
    [over,'-'] : ( [a,b] -- [a,{'-',b,a}] )
    [over,'+'] : ( [a,b] -- [a,{'+',b,a}] )
    [over,'*'] : ( [a,b] -- [a,{'*',b,a}] )
    [drop,dup] : ( [a,b] -- [a,a] )
    [drop,rot] : ( [a,b,c,d] -- [b,c,a] )
    [drop,over] : ( [a,b,c] -- [a,b,a] )
    [drop,drop] : ( [a,b] -- [] )
    [drop,swap] : ( [a,b,c] -- [b,a] )
    [drop,'-'] : ( [a,b,c] -- [{'-',a,b}] )
    [drop,'+'] : ( [a,b,c] -- [{'+',a,b}] )
    [drop,'*'] : ( [a,b,c] -- [{'*',a,b}] )
    [swap,dup] ( multiple ) ( [a,b] -- [b,a,a] )
    [swap,rot] : ( [a,b,c] -- [c,b,a] )
    [swap,over] : ( [a,b] -- [b,a,b] )
    [swap,drop] : ( [a,b] -- [b] )
    [swap,swap] = id
    [swap,'-'] : ( [a,b] -- [{'-',b,a}] )
    [swap,'+'] : ( [a,b] -- [{'+',b,a}] )
    [swap,'*'] : ( [a,b] -- [{'*',b,a}] )
    ['-',dup] : ( [a,b] -- [{'-',a,b},{'-',a,b}] )
    ['-',rot] : ( [a,b,c,d] -- [b,{'-',c,d},a] )
    ['-',over] : ( [a,b,c] -- [a,{'-',b,c},a] )
    ['-',drop] ( multiple ) ( [a,b] -- [] )
    ['-',swap] : ( [a,b,c] -- [{'-',b,c},a] )
    ['-','-'] : ( [a,b,c] -- [{'-',a,{'-',b,c}}] )
    ['-','+'] : ( [a,b,c] -- [{'+',a,{'-',b,c}}] )
    ['-','*'] : ( [a,b,c] -- [{'*',a,{'-',b,c}}] )
    ['+',dup] : ( [a,b] -- [{'+',a,b},{'+',a,b}] )
    ['+',rot] : ( [a,b,c,d] -- [b,{'+',c,d},a] )
    ['+',over] : ( [a,b,c] -- [a,{'+',b,c},a] )
    ['+',drop] ( multiple ) ( [a,b] -- [] )
    ['+',swap] : ( [a,b,c] -- [{'+',b,c},a] )
    ['+','-'] : ( [a,b,c] -- [{'-',a,{'+',b,c}}] )
    ['+','+'] : ( [a,b,c] -- [{'+',a,{'+',b,c}}] )
    ['+','*'] : ( [a,b,c] -- [{'*',a,{'+',b,c}}] )
    ['*',dup] : ( [a,b] -- [{'*',a,b},{'*',a,b}] )
    ['*',rot] : ( [a,b,c,d] -- [b,{'*',c,d},a] )
    ['*',over] : ( [a,b,c] -- [a,{'*',b,c},a] )
    ['*',drop] ( multiple ) ( [a,b] -- [] )
    ['*',swap] : ( [a,b,c] -- [{'*',b,c},a] )
    ['*','-'] : ( [a,b,c] -- [{'-',a,{'*',b,c}}] )
    ['*','+'] : ( [a,b,c] -- [{'+',a,{'*',b,c}}] )
    ['*','*'] : ( [a,b,c] -- [{'*',a,{'*',b,c}}] )

# Source format

The source format for chine code is currently in form of
the Erlang term format. A program consists of terms of
lists or straight terms terminated with dots.

Constants are written like

    {const, Integer}
    {const, Symbol}
    Integer             suger for {const,Integer}
    {string, String}    suger for {array,[{const,C1},...{const,Cn}]} )

Symbols may be introduced with

    {enum, ["Sym1", "Sym2" ... "SymN" ]}

This enumerates the symbols to values 0 ... N-1, later use
of {const,"Sym1"} will have the same effect as {const,0}.

    {label, L}

Is a way of marking a position in the code, it may be
used for relative or absolute addressing.

    {caddr,L}

Is used to create an absolute (actually relative from start of code)
value of a label address.

If a Label must be access from outside the code an export directive
will place the label symbol and it's value in the symbol table.

    {export, L} 


Comments may be inserted as

    {comment, "Text"}

Or comments may be given in the source file with Erlangs % comments.
There are a number of helpful constructs for structured programming
constructs such as loops and if statemensts

## Control

    {'if', Then}
    {'if', Then, Else} 

Note that if MUST be quoted since 'if' is a keyword in Erlang

    {'again', Loop}
    {'until', Loop}
    {'repeat', While, Loop}

In the above loops 'again' is an infine loop and 'until' evaluate the
Loop body until the result of the Loop body is a none
zero value. Note that the 'again' Loop body should not produce any
stack values since none are popped in the loop by it self.
'repeat' evaluate the code in the While part and if the
result is none zero the Loop is executed and the loop restarts.

    {'for',Loop}

'for' implements a finte loop where the loop count is expeced on the
stack on entry. The loop index is counted downward towards zero and
may be fetched during the loop using 'r@'.

## Arrays

Arrays can be used to access data more efficent, like

    {array, [{const,5},{const,7},{const,11},{const,13},{const,17}]}

Is a small array of prime numbers. The array construct compiles inline
and push the array pointer onto the stack, there is no penalty to
loop over an array construct, just a pointer being pushed and
the array data being skipped. So a loop over an array construct is
perfectly ok.
To access an item in the array the '[]' operation is used. Note that
the '[]' operation use a zero based index.

    [5, {for, [{array,[$o,$l,$l,$e,$H]},'r@','1-','[]',emit]}]
	
Arrays are also used for jump tables

    {array, [{caddr,L1}, {caddr,L2}, ..., {caddr,Ln}]}

To jump to the code and the label Li

	i-1, '[]', 'jmp*'

if the array is an array of labels to functions
then the 'execute' operation may be used. Control is being return after
function is done executing.

    i-1, '[]', execute

## Call frames (fenter/fleave)

When call frames are need, you are being lazy, or want to play
it safe then use fenter and fleave to avoid stack accounting.

        fenter <nlocals>

given a stack frame looking like (growing upwards!)

	sp -> | 3 |
	      | 2 |
	      | 1 |
	      | a |
	      | b |
        fp -> | x |
	      | y |

using fenter 4 will: them calculate of value 8 and 9

        rp: ( -- fp )          fp is save on return stack

       sp'' -> [ 9 ] ret 2
        sp' -> [ 8 | ret 1
	       [ 0 ] {arg,-4}  forth local
	       [ 0 ] {arg,-3}  third local
	       [ 0 ] {arg,-2}  second local
               [ 0 ] {arg,-1}  first local
     fp'=sp -> | 3 | {arg,0}
	       | 2 | {arg,1}
	       | 1 | {arg,2}
               | a |
	       | b |
        fp ->  | x |
	       | y |	       

fleave 2,3 will remove call frame and copy the return value(s) to
the previous call frame outputs:

        fp' -> [ 3 ]    (just show where the fp' was)
         sp -> | 9 |
	       | 8 |
               | a |
	       | b |
        fp ->  | x |
	       | y |

## Binary format

The Source code is transformed compiled/assembled into a binary
form that is divided into a couple of sections.

    'C','H','I','N'

is the magic 32 bit word that is used to recognise chine binary code.
Following the magic word is a small header

    Version:32     file version
    Crc:32         crc-32 over all data (while Crc was set to zero)
    Length:32      content length in number of bytes

Length cover the rest of the sections including the crc-32.
The Crc is calculated over all data with Crc field set to zero.

symbol table section

    'S','Y','M','B',
	Length:32,

Each entry in the symbol sections, looks like

    Len:8, Sym:Len/binary, N:8, Value:N/binary

The code follows the SYMB section in a section called CODE

	'C','O','D','E',
	Length:32
