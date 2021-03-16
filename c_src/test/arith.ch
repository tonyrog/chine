%% -*- erlang -*-
%%  Test arithmetic

{export,run}.

[
 {label,run},
 5, 3, '+', 8, '=', drop,
 5, 3, '-', 2, '=', drop,
 5, 3, '*', 15, '=', drop,
 5, 3, '/',  0, '=', drop,
 5, 4, 2, '*', '+', drop,
 5, 4, 2, '+', '*', drop,
 exit
].
