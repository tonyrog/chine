%% -*- erlang -*-

%% print a string

[
 %% this is the literal string
 {string,"Hello world\n"},
 {const,12},
 {for, [dup, {const,12}, 'r@', '-', '[]', emit]},
 {const,12},
 yield
].
