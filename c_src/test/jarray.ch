%% -*- erlang -*-
%%
{export, run}.
{import, "io"}.

{def, run,
 [
  {array, [{caddr,a}, {caddr,b}, {caddr,c}, {caddr,a}, {caddr,d}]}, 
  dup, size, {'_for', [dup, 'r@', '1-', dup, print, $=, putc, '[]', println]},
  2, '[]', 'jmp*',

  {label,a},
  {call, puts, [{string, "AAA"}]},
  terminate,

  {label,b},
  {call, puts, [{string, "BBBB"}]},
  terminate,

  {label,c},
  {call, puts, [{string, "CCCCC"}]},
  terminate,
  
  {label,d},
  {call, puts, [{string, "DDDDDD"}]},
  terminate
 ]}.


