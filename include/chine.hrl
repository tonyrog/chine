-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(FILE_VERSION, 16#01010000).

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
	  next,     %% (--RP[0]<0) 
	  jmplz,    %% (TOP < 0)
	  jmp,
	  call,
	  literal,
	  array
	 }).

-define(JOP(X), (#jopcode.X-2)).
-define(JENUM(X), X => (#jopcode.X-2)).

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
	  'not',
	  invert,
	  negate,
	  '/',
	  shift,
	  '!',
	  '@',
	  '>r',
	  'r>',
	  'r@',
	  exit,
	  sys,
	  yield,
	  '[]',
	  execute
	 }).
-define(OP(X), (#opcode.X-2)).
-define(ENUM(X), X => (#opcode.X-2)).


%% Failure codes
-define(FAIL_STACK_OVERFLOW,    -1).
-define(FAIL_STACK_UNDERFLOW,   -2).
-define(FAIL_RSTACK_OVERFLOW,   -3).
-define(FAIL_RSTACK_UNDERFLOW,  -4).
-define(FAIL_DIV_ZERO,          -5).
-define(FAIL_TIMER_OVERFLOW,    -6).
-define(FAIL_MEMORY_OVERFLOW,   -7).

-record(sys, {
	  sys_init,
	  sys_param_fetch,
	  sys_param_store,
	  sys_timer_init,
	  sys_timer_start,
	  sys_timer_stop,
	  sys_timer_timeout,
	  sys_timer_running,
	  sys_input_fetch,
	  sys_output_store,
	  sys_select_timer,
	  sys_deselect_timer,
	  sys_select_input,
	  sys_deselect_input,
	  sys_deselect_all,
	  sys_uart_send,
	  sys_uart_recv,
	  sys_uart_avail,
	  sys_now,
	  sys_gpio_input,
	  sys_gpio_output,
	  sys_gpio_set,
	  sys_gpio_clr,
	  sys_gpio_get,
	  sys_analog_send,
	  sys_analog_recv,
	  sys_can_send
	 }).
-define(SYS(N), (#sys.X-2)).
-define(SENUM(X), X => (#sys.X-2)).

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
