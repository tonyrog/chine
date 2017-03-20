%% -*- erlang -*-

%% print a string
{export,run}.
%% {export,init}.
%% {export,final}.

[
 %% this is the literal string
 {label,run},
 {string,"Hello world\n"},
 {const,12},
 {for, [dup, {const,12}, 'r@', '-', '[]', emit]},

 1234, {call,println},
 17, {call,print},$\s,emit, -17, {call,print}, $\n, emit,
 1, {call,print},$\s,emit, -1, {call,print}, $\n, emit,


 {again, yield},
{label,init},
 exit,
{label,final},
 exit
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
 dup, '0=',
 {'if', [drop,exit]},
 dup, 10,'/',{call,uprint},  %% print(n/10)
 10,mod,$0,'+',emit,
 exit
].
