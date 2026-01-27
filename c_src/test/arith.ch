%% -*- erlang -*-
%%  Test arithmetic

{export,run}.

{def, run, 
 [
 5, 3, '+', 8, '!=', {'_if', exit}, $+,emit,$\n,emit,
 5, 3, '-', 2, '!=', {'_if', exit}, $-,emit,$\n,emit,
 5, 3, '*', 15, '!=', {'_if', exit}, $*,emit,$\n,emit,
 5, 3, '/',  1, '!=', {'_if', exit}, $/,emit,$\n,emit,
 5, 4, 2, '*', '+', 13, '!=', {'_if', exit}, $*,emit,$+,emit,$\n,emit,
 5, 4, 2, '+', '*', 30, '!=', {'_if', exit}, $+,emit,$*,emit,$\n,emit,
 5, 3, 'mod',  2, '!=', {'_if', exit}, $m,emit,$o,emit,$d,emit,$\n,emit,
 $0,emit,$k,emit,$\n,emit,
 terminate
 ]}.
