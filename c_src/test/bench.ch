%% -*- erlang -*-
%% 
%% BENCHMARK calculate 12! 1000000 times
%% around 500-600ms on my mac
%%
[
 now,
 1000000,
 {call, bench},
 now,
 swap, '-', 
 {call, println},
 yield,

 %% BENCH : ( n -- )
 {label,bench},
 {label,bench_loop},
 12,
 {call, factorial},
 drop,
 '1-',
 dup, {jmpnz, bench_loop},
 drop,
 exit,
 
 %% FACTORIAL: ( n -- n! )
 {label,factorial},
 dup,       %% ( r n )
 {label,fact_loop},
 '1-',         %% ( r n )
 dup, {jmpz,fact_done},
 dup, rot,     %% ( n r n )
 '*', swap,    %% ( r*n n )
 {jmp, fact_loop},

 {label,fact_done},
 drop, exit,

 %% PRINTLN: ( n -- )
 {label,println},
 {call,print},
 $\n,emit,
 exit,

 %% PRINT: ( n -- )  reverse print number :-)
 {label,print},
 dup,
 {'if', [], [$0,emit,drop,exit]},
 {label, print_loop},
 dup,10,mod,  %% ( n d )
 $0,'+',emit, %% ( n )
 10,'/',      %% ( n/10 )
 dup, {jmpnz, print_loop},
 drop,
 exit
 
].
