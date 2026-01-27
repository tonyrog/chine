%% -*- erlang -*-
%% 
%% BENCHMARK calculate 12! 1000000 times
%%
{export,run}.

{def, run,
 [
  12, factorial, println,
  now,
  1000000,
  bench,
  now,
  swap, '-', 
  println,
  terminate
]}.

%% BENCH : ( n -- )
{def, bench, 
 [
  {label,bench_loop},
%%  dup, println,
  12, factorial, drop,
  '1-',
  dup, {jmpnz, bench_loop},
  drop
]}.

 %% FACTORIAL: ( n -- n! )
{def, factorial, 
 [ 
   dup,          %% ( r n )
   {label,fact_loop},
   '1-',         %% ( r n )
   dup, {jmpz,fact_done},
   dup, rot,     %% ( n r n )
   '*', swap,    %% ( r*n n )
   {jmp, fact_loop},
   
   {label,fact_done},
   drop
]}.

%% PRINTLN: ( n -- )
{def, println, 
 [
  print,$\n,emit
]}.

%% PRINT: ( n -- )
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
  dup, 10, mod,   %% 123 123 10 % = 123 3
  swap,           %% 3 123
  10,'/',         %% 3 12
  uprint,
  $0,'+',emit
]}.
