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
   dup, -16#8000, '<', {'if', [2#100000, {call,op1_}, {call,comma4}, exit]},
   dup,  16#7fff, '>', {'if', [2#100000, {call,op1_}, {call,comma4}, exit ]},
   dup, -16#80,   '<', {'if', [2#010000, {call,op1_}, {call,comma2}, exit ]},
   dup,  16#7f,   '>', {'if', [2#010000, {call,op1_}, {call,comma2}, exit ]},
   2#0000000, {call,op1_},comma
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
 

{def, j_dup,   [ {const, dup},  {call,op0} ]}.
{def, j_rot,   [ {const, rot},  {call,op0} ]}.
{def, j_over,  [ {const, over}, {call,op0} ]}.
{def, j_drop,  [ {const, drop}, {call,op0} ]}.
{def, j_swap,  [ {const, swap}, {call,op0} ]}.
{def, 'j_-',   [ {const, '-'},  {call,op0} ]}.
{def, 'j_+',   [ {const, '+'},  {call,op0} ]}.
{def, 'j_*',   [ {const, '*'},  {call,op0} ]}.

{def, j_nop,   [ {const, nop},  {call,op0} ]}.

{def, j_short_jmp, [ {const, jmp}, {call,op2} ]}.
{def, j_long_jmp,  [ {const, jmp}, {call,op1} ]}.

%% dup_swap dup then swap
{def, j_dup_swap, [{const,dup},{const,swap},{call,op3}]}.

{def, run, 
[
 here,  %% push original dp pointer
 {call, compile},
 dup, here, swap, '-',  %% dp0 n
 dup, {call, println},  %% print size
 {call, xdump},
 $\n, emit,
 terminate]}.

{def, compile,
[
 {call, j_dup},  $0, emit, $\n, emit,
 {call, j_rot},  $1, emit, $\n, emit,
 {call, j_over}, $2, emit, $\n, emit,
 {call, j_drop}, $3, emit, $\n, emit,
 {call, j_swap}, $4, emit, $\n, emit,
 {call, 'j_-'},  $5, emit, $\n, emit,
 {call, 'j_+'},  $6, emit, $\n, emit,
 {call, 'j_*'},  $7, emit, $\n, emit,
 %% 0b11000100 = 0xC4
 {call, j_dup_swap}, $C, emit, $4, emit, $\n, emit, 
 %% 0b10010100 = 0x94
 2, {call, j_short_jmp}, $9, emit, $4, emit, $\n, emit,
 %% 0b01010100 = 0x54 0x03 0xE8
 1000, {call, j_long_jmp}, $X, emit, $\n, emit,
 nop
]}.

%% PRINTLN: ( n -- )
{def, println,
[
 {call,print},
 $\n,emit
]}.

%% print: ( n -- )
{def, print,
[
 dup, '0=',
 {'if', [$0,emit,drop,exit]},
 dup, '0<',
 {'if', [$-,emit,negate]},
 {call,uprint}
]}.

%% UPRINT: ( n -- ) n>=0
{def, uprint,
 [
  dup, '0=', {'if', [drop,exit]},
  dup, 10, mod,
  swap,
  10,'/',
  {call,uprint},
  $0,'+',emit
 ]}.

%% c-addr n 
{def, xdump,
[
 {for, [dup, 'c@', {call,xemit8}, $\s, emit, '1+']}
]}.

%% xemit8: ( n -- )
{def, xemit8,
[
 dup, -4, shift, {call,xemit4},
 {call,xemit4}
]}.

%% xemit4: ( n -- ) 
%% emit hex digit 0-f
{def, xemit4,
 [
  16#f, 'and',
  dup, 9, '>', {'if', [10, '-', $a, '+', emit, exit]},
  $0, '+', emit
]}.
  
