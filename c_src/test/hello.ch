%% -*- erlang -*-

{export,run}.

%% this is the literal string
[{label,run},
 {string,"Hello world\r\n"},
 13, {'_for', [dup, 13, 'r@', '-', '[]', emit]},
 terminate
].
