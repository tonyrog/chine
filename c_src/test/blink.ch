%% -*- erlang -*-

%% blink with arduino leds

[
 13,gpio_output,
 1,timer_init,
 2,timer_init,
 16#2710, 1, 500, 'param!',
 16#2710, 2, 750, 'param!',

 {label, loop},
 13, gpio_set,
 $1, emit,
 1, timer_start,

 {label,wait1},
 1, timer_timeout,
 'not', {'if', [1, select_timer, yield, {jmp,wait1}]},
 1, deselect_timer,
 
 13, gpio_clr,
 $0, emit,
 2, timer_start,

 {label,wait2},
 2, timer_timeout,
 'not',{'if', [2, select_timer, yield, {jmp,wait2}]},
 2, deselect_timer,

 {jmp, loop}
].
