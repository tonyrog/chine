-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(OPCODE1(X), ((X) band 0x7f)).
-define(OPCODE2(A,B), (16#80 bor (((B) band 15) bsl 3) bor ((A) band 7))).

-record(opcode, {
	  %% op3 0..7
	  'zbranch.h',
	  'literal.h',
	  dup,
	  rot,
	  over,
	  drop,
	  swap,
	  '-',
	  %% op4 8..15
	  '+',
	  '*',
	  negate,
	  'and',
	  'or',
	  '0=',
	  '0<',
	  'not',
	  %% op7 ..
	  'nop',
	  'u<',
	  'u<=',
	  'xor',
	  '/',
	  'invert',
	  'lshift',
	  'rshift',
	  '!',
	  '@',
	  'ret',
	  'literal.w',
	  'literal.l',
	  'branch.b',
	  'branch.w',
	  'zbranch.w',
	  'ibranch.b',
	  'ibranch.w',
	  'call.b',
	  'call.w',
	  'sys.b',
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

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
