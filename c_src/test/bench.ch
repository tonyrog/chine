%% -*- erlang -*-
%% 
%% BENCHMARK calculate 12! 1000000 times
%%
{export,run}.

[
 {label,run},
 %% 12345, {call, uprint}, $\n, emit,
 %% terminate, 
 12, {call, factorial}, {call, println},

 now,
 1000000,
 {call, bench},
 now,
 swap, '-', 
 {call, println},
 terminate
].

[
 %% BENCH : ( n -- )
 {label,bench},
 {label,bench_loop},
 12, {call, factorial}, drop,
 '1-',
 dup, {jmpnz, bench_loop},
 drop,
 exit
].


[ 
 %% FACTORIAL: ( n -- n! )
 {label,factorial},
 dup,          %% ( r n )
 {label,fact_loop},
 '1-',         %% ( r n )
 dup, {jmpz,fact_done},
 dup, rot,     %% ( n r n )
 '*', swap,    %% ( r*n n )
 {jmp, fact_loop},

 {label,fact_done},
 drop, exit
].


%% PRINTLN: ( n -- )
[
 {label,println},
 {call,print},
 $\n,emit,
 exit
].

%% PRINT: ( n -- )
[
 {label,print},
 dup, '0=',
 {'if', [$0,emit,drop,exit]},
 dup, '0<',
 {'if', [$-,emit,negate]},
 {call,uprint},
 exit
].

%% UPRINT: ( n -- ) n>=0
[
 {label,uprint},
 dup, '0=', {'if', [drop,exit]},
 dup, 10, mod,   %% 123 123 10 % = 123 3
 swap,           %% 3 123
 10,'/',         %% 3 12
 {call,uprint},
 $0,'+',emit,
 exit
].
