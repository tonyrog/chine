%% -*- erlang -*-
%%
{export, run}.
{import, "io"}.

[{label, run},
 {array, [1,2,3,4,5,6,7]},
 dup, 0, '[]', println,
 size, println,

 {array, [10,20,30,40,50,60]},
 dup, 1, '[]', println,
 size, println,

 {array, [100,200,300,400,500]},
 dup, 2, '[]', println,
 size, println,

 {array, [10000,20000,30000,40000]},
 dup,3, '[]', println,
 size, println,

 terminate
].


