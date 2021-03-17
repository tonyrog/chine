%% -*- erlang -*-

{export,run}.

[
 %% this is the literal string
 {label,run},
 {string,"Hello world\n"},
 {const,12},
 {for, [dup, {const,12}, 'r@', '-', '[]', emit]},
 terminate 
].
