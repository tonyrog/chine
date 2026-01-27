-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(FILE_VERSION, 16#01010000).

%% memory addresses 32-bit
-define(PAGE0_ADDR, 16#00000000).
-define(PAGE1_ADDR, 16#20000000).
-define(PAGE2_ADDR, 16#40000000).
-define(PAGE3_ADDR, 16#60000000).
-define(PAGE4_ADDR, 16#80000000).
-define(PAGE5_ADDR, 16#A0000000).
-define(PAGE6_ADDR, 16#C0000000).
-define(PAGE7_ADDR, 16#E0000000).

-define(ZERO_START, ?PAGE0_ADDR).
-define(CODE_START, ?PAGE1_ADDR).
-define(RAM_START,  ?PAGE4_ADDR).

-define(MAX_ARGS,    255).
-define(MAX_RETURNS, 15).
-define(MAX_LOCALS,  65535).
-define(CELL_SIZE,   4).  %% 32-bit

-define(RAM_CELL(I), (?RAM_START+I*?CELL_SIZE)).
-define(DP,  ?RAM_CELL(0)).
-define(TIB, ?RAM_CELL(1)).
-define(IN,  ?RAM_CELL(2)).
-define(TIB_SIZE, 81).

-define(OPCODE0(OP),     <<0:2, (OP):6>>).
%% L = 0 (1 byte arg) 1 = (2 byte arg)  2 = (4 bytes arg)
-define(OPCODE1(JOP,L),  <<1:2, (L):2, (JOP):4>>).
%% A is a 3 bit signed integer
-define(OPCODE2(JOP,A),  <<2:2, (A):3, (JOP):3>>).
%% Combine opcode1 and opcode2 (in range 0..7) into one byte
-define(OPCODE3(OP2,OP1), <<3:2, (OP2):3, (OP1):3>>).

-define(CHINE_TRUE, -1).
-define(CHINE_FALSE, 0).

-record(jopcode, 
	{
	 %% OPCODE1 and OPCODE2
	 jmpz,     %% (TOS == 0)
	 jmpnz,    %% (TOS != 0)
	 next,     %% (--RP[0]<0) 
	 jmplz,    %% (TOS < 0)
	 jmp,
	 call,
	 literal,
	 '_jop_7',
	 %% OPCODE1 only
	 get,     %% 8  get element in stack frame
	 array,   %% 9
	 enter,   %% 10 enter stack frame
	 leave,   %% 11 leave stack frame
	 set,     %% 12 set element in stack frame
	 jop_13,
	 jop_14,
	 jop_15
	}).

-define(ARRAY, 9).


-define(JOP(X), (#jopcode.X-2)).
-define(JENUM(X), X => (#jopcode.X-2)).

-record(opcode, 
	{
	 dup,
	 rot,
	 over,
	 drop,
	 swap,
	 '-',
	 '+',
	 '*',
	 %% -----
	 'nop',
	 'and',
	 'or',
	 'xor',
	 '0=',
	 '0<',
	 'not',
	 '_op_15',
	 negate,
	 '/',
	 shift,
	 '!',
	 '@',
	 '>r',
	 'r>',
	 'r@',
	 exit,
	 sys,    %% 25
	 yield,
	 '[]',
	 execute,
	 'fp@',
	 'fp!',
	 'sp@',
	 'sp!',
	 'c!',
	 'c@',
	 'size'
	}).
-define(OP(X), ((#opcode.X)-2)).
-define(ENUM(X), X => ((#opcode.X)-2)).

-define(SYS, 25).


%% Failure codes
-define(FAIL_STACK_OVERFLOW,    -1).
-define(FAIL_STACK_UNDERFLOW,   -2).
-define(FAIL_RSTACK_OVERFLOW,   -3).
-define(FAIL_RSTACK_UNDERFLOW,  -4).
-define(FAIL_DIV_ZERO,          -5).
-define(FAIL_TIMER_OVERFLOW,    -6).
-define(FAIL_MEMORY_OVERFLOW,   -7).

-record(sys, 
	{
	 sys_init,
	 sys_terminate,
	 sys_now,
	 sys_emit,
	 sys_recv,
	 sys_avail,
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
	 sys_uart_connect,
	 sys_uart_send,
	 sys_uart_recv,
	 sys_uart_avail,
	 sys_uart_disconnect,
	 sys_gpio_input,
	 sys_gpio_output,
	 sys_gpio_set,
	 sys_gpio_clr,
	 sys_gpio_get,
	 sys_analog_send,
	 sys_analog_recv,
	 sys_can_connect,
	 sys_can_send,
	 sys_can_recv,
	 sys_can_avail,
	 sys_can_disconnect,
	 sys_file_open,
	 sys_file_write,
	 sys_file_read,
	 sys_file_close,
	 sys_file_seek
	}).
-define(SYS(N), (#sys.X-2)).
-define(SENUM(X), X => (#sys.X-2)).

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
