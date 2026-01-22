%% -*- erlang -*-

%% opcodes for 2#00 and 2#11 (op0 and op3)
{enum, [
	dup, rot, over, drop, swap, '-', '+', '*',
	'nop', 'and', 'or', 'xor', '0=', '0<', 'not', negate,
	'/', shift, '!', '@', '>r', 'r>', 'r@', exit, sys, yield,
	'[]', 'execute', 'fp@', 'fp!', 'sp@', 'sp!', 'c!', 'c@']}.


%% op0 : ( xxxxxx -- )  ( opcode in range 0-63 )
%% example: {const,'+'}, op0

{def, op0,
 [16#3f, 'and', comma]}.

%% op3 : ( xxx yyy -- )  ( xxx/yyy in range 0-7 ) 
%% build instruction 11xxxyyy
{def, op3,
 [
  swap,               %%  -- yyy xxx
  16#7, 'and',        %%  -- yyy 000xxx
  3, shift,           %%  -- yyy xxx000
  swap, 16#7, 'and',  %%  -- xxx000 000yyy
  'or',               %%  -- xxxyyy
  16#c0, 'or',        %%  -- 11xxxyyy
  comma ]}.

%% opcodes for 2#01 and 2#10 (op1 and op2)
{enum, [ %% op1/op2 
	 jmpz, jmpnz, next, jmplz, jmp, call, literal, array,
	 %% op1
	 arg
       ]}.

%% op2 : ( aaa jjj -- )  ( aaa in range 0-7, jjj is 3 bit opcode ) 
%% example: {const,jmpz}, -1, op2,
%% build instruction 10aaajjj
{def, op2,
 [
  swap,
  16#7, 'and', 3, shift,    %%  -- jjj aaa000
  swap, 16#7, 'and', 'or',  %%  -- aaajjj
  16#80, 'or',              %%  -- 10aaajjj
  comma ]}.

%% op1 : ( arg jjjj -- )  ( arg is an integer, jjjj is 4 bit opcode ) 
%% example: {const,jmpz}, -10000, op1,
%% build instruction 01eejjjj <arg:8/16/32>
{def, op1, 
 [ 
   swap,
   dup, -16#8000, '<', {'_if', [2#100000, op1_, comma4, exit]},
   dup,  16#7fff, '>', {'_if', [2#100000, op1_, comma4, exit ]},
   dup, -16#80,   '<', {'_if', [2#010000, op1_, comma2, exit ]},
   dup,  16#7f,   '>', {'_if', [2#010000, op1_, comma2, exit ]},
   2#0000000, op1_, comma
]}.

%% code1 : ( jjjj arg ee0000 -- arg )
%% write the opcode 01eejjjj
%% 
{def, op1_,
 [
  rot,           %% -- arg ee0000 jjjj
  16#f, 'and',   %% -- arg ee0000 00jjjj
  'or',          %% -- arg eejjjj
  16#40, 'or',   %% -- arg 01eejjjj
  comma          %% -- arg
 ]}.

%% comma2 : ( i16 -- )
{def, comma2, 
 [
  dup, -8, shift, comma, comma]}.

%% comma4 : ( i32 -- )
{def, comma4, 
[ dup, -24, shift, comma,
  dup, -16, shift, comma,
  dup, -8, shift, comma,
  comma]}.
 

{def, j_dup,   [ {const, dup},  op0 ]}.
{def, j_rot,   [ {const, rot},  op0 ]}.
{def, j_over,  [ {const, over}, op0 ]}.
{def, j_drop,  [ {const, drop}, op0 ]}.
{def, j_swap,  [ {const, swap}, op0 ]}.
{def, 'j_-',   [ {const, '-'},  op0 ]}.
{def, 'j_+',   [ {const, '+'},  op0 ]}.
{def, 'j_*',   [ {const, '*'},  op0 ]}.

{def, j_nop,   [ {const, nop}, op0 ]}.

{def, j_short_jmp, [ {const, jmp}, op2 ]}.
{def, j_long_jmp,  [ {const, jmp}, op1 ]}.

%% dup_swap dup then swap
{def, j_dup_swap, [{const,dup},{const,swap},op3]}.

{export, run}.

{def, run, 
[
 here,  %% push original dp pointer
 compile,
 dup, here, swap, '-',  %% dp0 n
 dup, println,  %% print size
 xdump,
 $\n, emit,
 terminate]}.

{def, compile,
[
 j_dup,  $0, emit, $\n, emit,
 j_rot,  $1, emit, $\n, emit,
 j_over, $2, emit, $\n, emit,
 j_drop, $3, emit, $\n, emit,
 j_swap, $4, emit, $\n, emit,
 'j_-',  $5, emit, $\n, emit,
 'j_+',  $6, emit, $\n, emit,
 'j_*',  $7, emit, $\n, emit,
 %% 0b11000100 = 0xC4
 j_dup_swap, $C, emit, $4, emit, $\n, emit, 
 %% 0b10010100 = 0x94
 2, j_short_jmp, $9, emit, $4, emit, $\n, emit,
 %% 0b01010100 = 0x54 0x03 0xE8
 1000, j_long_jmp, $X, emit, $\n, emit,
 nop
]}.

%% PRINTLN: ( n -- )
{def, println,
[
 print,
 $\n,emit
]}.

%% print: ( n -- )
{def, print,
[
 dup, '0=',
 {'_if', [$0,emit,drop,exit]},
 dup, '0<',
 {'_if', [$-,emit,negate]},
 {call,uprint}
]}.

%% UPRINT: ( n -- ) n>=0
{def, uprint,
 [
  dup, '0=', {'_if', [drop,exit]},
  dup, 10, mod,
  swap,
  10,'/',
  uprint,
  $0,'+',emit
 ]}.

%% c-addr n 
{def, xdump,
[
 {'_for', [dup, 'c@', xemit8, $\s, emit, '1+']}
]}.

%% xemit8: ( n -- )
{def, xemit8,
[
 dup, -4, shift, xemit4,
 xemit4
]}.

%% xemit4: ( n -- ) 
%% emit hex digit 0-f
{def, xemit4,
 [
  16#f, 'and',
  dup, 9, '>', {'_if', [10, '-', $a, '+', emit, exit]},
  $0, '+', emit
]}.
  
