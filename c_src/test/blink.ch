%% -*- erlang -*-

%% blink with arduino leds

{enum, ["state_mem"]}.
{enum, ["init", "on", "off"]}.

{export,init}.
{export,run}.

{def, init, 
[
 13,gpio_output,
 1,timer_init,
 2,timer_init,
 16#2710, 1, 500, 'param!',
 16#2710, 2, 750, 'param!',
 {const,"init"},{const,"state_mem"},'!'
]}.

{def, run, 
 [
  {array, [{caddr,"state_init"},{caddr,"state_on"},{caddr,"state_off"}]},
  {const, "state_mem"}, '@', '[]', 'jmp*',
 
  {label,"state_on"},
  1, timer_timeout, 'not', {'_if', [1, select_timer, exit]},
  1, deselect_timer,
  13, gpio_clr,
  $0, emit,
  0, timer_start,
  0, select_timer,
  {const,"off"},{const,"state_mem"},'!',
  exit,

  {label,"state_off"},
  0, timer_timeout, 'not', {'_if', [0, select_timer, exit]},
  0, deselect_timer,

  {label,"state_init"},
  13, gpio_set,
  $1, emit,
  1, timer_start,
  1, select_timer,
  {const,"on"},{const,"state_mem"},'!',
  exit
]}.

