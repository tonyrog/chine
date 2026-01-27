%% -*- erlang -*-

%% opcodes for 2#00 and 2#11 (op0 and op3)
{enum, [
	dup, rot, over, drop, swap, '-', '+', '*',
	'nop', 'and', 'or', 'xor', '0=', '0<', 'not', '_op_15', negate,
	'/', shift, '!', '@', '>r', 'r>', 'r@', exit, sys, yield,
	'[]', 'execute', 'fp@', 'fp!', 'sp@', 'sp!', 'c!', 'c@', 'size']}.

%% sys calls
{enum, [
	sys_init,
	sys_terminate,
	sys_now, sys_emit, sys_recv, sys_avail, 
	sys_param_fetch, sys_param_store, 
	sys_timer_init, sys_timer_start, sys_timer_stop,
	sys_timer_timeout, sys_timer_running,
	sys_input_fetch, 
	sys_output_store,
	sys_select_timer,
	sys_deselect_timer,
	sys_select_input,
	sys_deselect_input,
	sys_deselect_all,
	sys_uart_connect, sys_uart_send, sys_uart_recv, sys_uart_avail,
	sys_uart_disconnect,
	sys_gpio_input, sys_gpio_output, sys_gpio_set, sys_gpio_clr,
	sys_analog_send, sys_analog_recv,
	sys_can_connect, sys_can_send, sys_can_recv, sys_can_avail,
	sys_can_disconnect,
	sys_file_open, sys_file_write, sys_file_read, sys_file_close,
	sys_file_seek]}.

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
	 jmpz, jmpnz, next, jmplz, jmp, call, literal, '_jop_7',
	 %% op1
	 get,
	 array,
	 enter,
	 leave,
	 set
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

%% comma2 : ( i16 -- ) (little endian!)
{def, comma2, 
 [
  dup, comma,
  -8, shift, comma]}.

%% comma4 : ( i32 -- ) (little endian!)
{def, comma4, 
 [ dup, comma,
   dup, -8, shift, comma,
   dup, -16, shift, comma,
   -24, shift, comma]}.
 
{def, j_dup,   [ {const, dup},  op0 ]}.
{def, j_rot,   [ {const, rot},  op0 ]}.
{def, j_over,  [ {const, over}, op0 ]}.
{def, j_drop,  [ {const, drop}, op0 ]}.
{def, j_swap,  [ {const, swap}, op0 ]}.
{def, 'j_-',   [ {const, '-'},  op0 ]}.
{def, 'j_+',   [ {const, '+'},  op0 ]}.
{def, 'j_*',   [ {const, '*'},  op0 ]}.
{def, 'j_nop', [ {const, 'nop'}, op0 ]}.
{def, 'j_and', [ {const, 'and'}, op0 ]}.
{def, 'j_or', [ {const, 'or'}, op0 ]}.
{def, 'j_xor', [ {const, 'xor'}, op0 ]}.
{def, 'j_0=', [ {const, '0='}, op0 ]}.
{def, 'j_0<', [ {const, '0<'}, op0 ]}.
{def, 'j_not', [ {const, 'not'}, op0 ]}.
{def, 'j_negate', [ {const, 'negate'}, op0 ]}.
{def, 'j_/', [ {const, '/'}, op0 ]}.
{def, 'j_shift', [ {const, 'shift'}, op0 ]}.
{def, 'j_!', [ {const, '!'}, op0 ]}.
{def, 'j_@', [ {const, '@'}, op0 ]}.
{def, 'j_>r', [ {const, '>r'}, op0 ]}.
{def, 'j_r>', [ {const, 'r>'}, op0 ]}.
{def, 'j_r@', [ {const, 'r@'}, op0 ]}.
{def, 'j_exit', [ {const, 'exit'}, op0 ]}.
{def, 'j_sys', [ {const, 'sys'}, op0 ]}.
{def, 'j_yield', [ {const, 'yield'}, op0 ]}.
{def, 'j_[]', [ {const, '[]'}, op0 ]}.
{def, 'j_execute', [ {const, 'execute'}, op0 ]}.
{def, 'j_fp@', [ {const, 'fp@'}, op0 ]}.
{def, 'j_fp!', [ {const, 'fp!'}, op0 ]}.
{def, 'j_sp@', [ {const, 'sp@'}, op0 ]}.
{def, 'j_sp!', [ {const, 'sp!'}, op0 ]}.
{def, 'j_c!', [ {const, 'c!'}, op0 ]}.
{def, 'j_c@', [ {const, 'c@'}, op0 ]}.
{def, 'j_size', [ {const, 'size'}, op0 ]}.

{def, 'j_literal', [ {const, literal}, op1 ]}.
{def, 'j_emit',    [ 'j_sys', {const, sys_emit}, comma] }.
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

 %% Compile some code and execute
 %% print "HELLO"
 here,  %% push current
 dup, printxln,  %% print dp address in hex

 compile_hello,  %% compile into here

 dup, printxln,         %% ( hello )
 dup,                   %% ( hello hello )

 dup, here, swap, '-',  %% ( hello hello n )
 dup, println,          %% print size
 xdump,                 %% ( hello )
 $\n, emit,

 dup, printxln,  %% print dp address in hex 
 execute,        %% run original dp
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
 %% 0b01010100 = 0x54 0xE8 0x03
 1000, j_long_jmp, $X, emit, $\n, emit,

 nop
]}.

{def, compile_hello,
 [
  $H, j_literal, j_emit,
  $E, j_literal, j_emit,
  $L, j_literal, j_emit,
  $L, j_literal, j_emit,
  $O, j_literal, j_emit,
  j_exit]}.

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
 uprint
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

%% ( c-addr n  -- )
{def, xdump,
[
 {'_for', [dup, 'c@', xemit8, $\s, emit, '1+']},
 drop
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

%% PRINTXLN: ( n -- )
{def, printxln,
[
 printx,
 $\n,emit
]}.
  
%% printx: ( n -- )
{def, printx,
[
 dup, -24, shift, xemit8,
 dup, -16, shift, xemit8,
 dup, -8, shift, xemit8,
 xemit8
]}.
