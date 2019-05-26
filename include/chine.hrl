-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(FILE_VERSION, 16#01010000).

-define(OPCODE0(OP),     <<0:2, (OP):6>>).
%% L = 0 (1 byte arg) 1 = (2 byte arg)  2 = (4 bytes arg)
-define(OPCODE1(JOP,L),  <<1:2, (L):2, (JOP):4>>).
%% A is a 3 bit signed integer
-define(OPCODE2(JOP,A),  <<2:2, (A):3, (JOP):3>>).
%% Combne opcode1 and opcode2 (in range 0..7) into one byte
-define(OPCODE3(OP1,OP2), <<3:2, (OP2):3, (OP1):3>>).

-define(CHINE_TRUE, -1).
-define(CHINE_FALSE, 0).

-record(jopcode, 
	{
	 %% OPCODE1 and OPCODE2
	 jmpz,     %% (TOP == 0)
	 jmpnz,    %% (TOP != 0)
	 next,     %% (--RP[0]<0) 
	 jmplz,    %% (TOP < 0)
	 jmp,
	 call,
	 literal,
	 array,
	 %% OPCODE1 only
	 arg
	}).

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
	 %% op6
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
	 sys,
	 yield,
	 '[]',
	 execute,
	 'fp@',
	 'fp!',
	 'sp@',
	 'sp!'
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
	 sys_file_close
	}).
-define(SYS(N), (#sys.X-2)).
-define(SENUM(X), X => (#sys.X-2)).

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
