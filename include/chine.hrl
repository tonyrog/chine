-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(OPCODE0(OP),     (16#00 bor ((OP) band 31))).
%% K = 0 (1 byte arg) 1 = (2 byte arg)  3 = (4 bytes arg)
-define(OPCODE1(JOP,K),  (16#40 bor ((JOP) band 7) bor (((K) band 7) bsl 3))).
%% X == 3 bit signed integer
-define(OPCODE2(JOP,X),  (16#80 bor ((JOP) band 7) bor (((X) band 7) bsl 3))).
%% Combne opcode1 and opcode2 (in range 0..7) into one byte
-define(OPCODE3(OP1,OP2),(16#c0 bor ((OP1) band 7) bor (((OP2) band 7) bsl 3))).

-record(jopcode, {
	  jmpz,     %% (TOP == 0)
	  jmpnz,    %% (TOP != 0)
	  jmpgtz,   %% (TOP > 0)
	  jmpgez,   %% (TOP >= 0)
	  jmp,
	  jmpi,
	  call,
	  literal
	 }).

-record(opcode, {
	  dup,
	  rot,
	  over,
	  drop,
	  swap,
	  '-',
	  '+',
	  '*',
	  %% op6
	  'nop',
	  'and',
	  'or',
	  'xor',
	  '0=',
	  '0<',
	  '0<=',
	  'u<',
	  'u<=',
	  'not',
	  invert,
	  negate,
	  '/',
	  lshift,
	  rshift,
	  '!',
	  '@',
	  ret,
	  sys,
	  exit,
	  yield
	 }).


%% Failure codes
-define(FAIL_STACK_OVERFLOW,    -1).
-define(FAIL_STACK_UNDERFLOW,   -2).
-define(FAIL_RSTACK_OVERFLOW,   -3).
-define(FAIL_RSTACK_UNDERFLOW,  -4).
-define(FAIL_DIV_ZERO,          -5).
-define(FAIL_TIMER_OVERFLOW,    -6).
-define(FAIL_MEMORY_OVERFLOW,   -7).

%% SYSTEM CALLS
-define(SYS_PARAM_FETCH,   1).  %% ( i s -- n )
-define(SYS_PARAM_STORE,   2).  %% ( v i s -- )
-define(SYS_TIMER_INIT,    3).  %% ( i --  )
-define(SYS_TIMER_START,   4).  %% ( i time --  )
-define(SYS_TIMER_STOP,    5).  %% ( i --  )
-define(SYS_TIMER_TIMEOUT, 6).  %% ( i -- f )
-define(SYS_TIMER_RUNNING, 7).  %% ( i -- f )
-define(SYS_INPUT_FETCH,   8).  %% ( i k -- n )
-define(SYS_SELECT_TIMER,  9).  %% ( i -- )
-define(SYS_SELECT_INPUT, 10).  %% ( i -- )
-define(SYS_DESELECT_ALL, 11).  %% ( -- )
-define(SYS_EMIT,         12).  %% ( c -- )
-define(SYS_KEY,          13).  %% (   -- c )
-define(SYS_QKEY,         14).  %% (   -- f )
-define(SYS_NOW,          15).  %% ( -- u )
%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
