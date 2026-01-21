%% -*- erlang -*-
%% write "Hello world!\n" to a file "hello.txt"
%%
{enum, ["O_READ", "O_WRITE", "O_RDWR"]}.
%% {define, "O_CREAT", 8#100}.

{export,run}.
{def, run, 
 [
  {string, "hello.txt"}, {const,"O_WRITE"}, 8#100, '+', file_open,
  {'_if', [ {string, "Hello world!\n"}, 13, file_write ],
   [ drop, 2, {string, "open error\n"}, 11, file_write ]},
  terminate
]}.
