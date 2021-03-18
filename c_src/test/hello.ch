%% -*- erlang -*-

{export,run}.

[
 %% this is the literal string
 {label,run},
 {string,"Hello world\r\n"},
 {const,13},
 {for, [dup, {const,13}, 'r@', '-', '[]', emit]},
 terminate 
].
