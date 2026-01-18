%% -*- erlang -*-
%%  Test arithmetic

{export,run}.

[
 {label,run},
 5, 3, '+', 8, '!=', {'if', exit}, $+,emit,$\n,emit,
 5, 3, '-', 2, '!=', {'if', exit}, $-,emit,$\n,emit,
 5, 3, '*', 15, '!=', {'if', exit}, $*,emit,$\n,emit,
 5, 3, '/',  1, '!=', {'if', exit}, $/,emit,$\n,emit,
 5, 4, 2, '*', '+', 13, '!=', {'if', exit}, $*,emit,$+,emit,$\n,emit,
 5, 4, 2, '+', '*', 30, '!=', {'if', exit}, $+,emit,$*,emit,$\n,emit,
 5, 3, 'mod',  2, '!=', {'if', exit}, $m,emit,$o,emit,$d,emit,$\n,emit,
 $0,emit,$k,emit,$\n,emit,
 terminate
].
