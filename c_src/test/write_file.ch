%% -*- erlang -*-
%% write "Hello world!\n" to a file "hello.txt"
%%
{enum, ["O_READ", "O_WRITE", "O_RDWR"]}.
%% {define, "O_CREAT", 8#100}.

[
 {export,run},
 {label,run},
 {string, "hello.txt\0"}, {const,"O_WRITE"}, {const,8#100}, '+', file_open,
 {'if', [ {string, "Hello world!\n\0"}, {const, 13}, file_write ],
  [ drop, {const,2}, {string, "open error\n\0"}, {const, 11}, file_write ]},
 terminate
].

