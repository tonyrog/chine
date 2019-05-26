%% -*- erlang -*-

{export,run}.

[
 {label,run},
 {const,2}, {const,3}, {const,5}, {call, func},
 {call, println},
 terminate
].

[
 {label, func},
 fenter,
 {arg,0},{arg,1},'*',{arg,2},'*',
 {call, println},
 fleave,
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
