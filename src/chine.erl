%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Compile & Assemble chine code
%%% @end
%%% Created : 25 Dec 2016 by Tony Rogvall <tony@rogvall.se>

-module(chine).

-export([asm/1, asm_file/1]).


-compile(export_all).
-export([effect/1, minmax_depth/1]).
-export([print_stack_effect/2]).
-export([opcodes/0]).

-include("../include/chine.hrl").

-define(OP(X), (#opcode.X-2)).
-define(ENUM(X), X => (#opcode.X-2)).
-define(MAP(X,Y), X => (#opcode.Y-2)).

-define(JOP(X), (#jopcode.X-2)).
-define(JENUM(X), X => (#jopcode.X-2)).
-define(JMAP(X,Y), X => (#jopcode.Y-2)).

%% on input a jump should look like {jmp,L} when L is a label
%% first pass transform all jumps into generic form {{jop,jmp},L}
%% later pass will determin the jump offset size K {{jop,jmp,K},L}
%% where K=0,1,2,4

jopcodes() ->
    #{
       ?JENUM(jmpz),
       ?JENUM(jmpnz),
       ?JENUM(jmpgtz),
       ?JENUM(jmpgez),
       ?JENUM(jmp),
       ?JENUM(jmpi),
       ?JENUM(call),
       ?JENUM(literal)
     }.

opcodes() ->
    #{
       ?ENUM(dup),
       ?ENUM(rot),
       ?ENUM(over),
       ?ENUM(drop),
       ?ENUM(swap),
       ?ENUM('-'),
       ?ENUM('+'),
       ?ENUM('*'),
       %% op6
       ?ENUM('nop'),
       ?ENUM('and'),
       ?ENUM('or'),
       ?ENUM('xor'),
       ?ENUM('0='),
       ?ENUM('0<'),
       ?ENUM('0<='),
       ?ENUM('u<'),
       ?ENUM('u<='),
       ?ENUM('not'),
       ?ENUM(invert),
       ?ENUM(negate),
       ?ENUM('/'),
       ?ENUM(lshift),
       ?ENUM(rshift),
       ?ENUM('!'),
       ?ENUM('@'),
       ?ENUM(ret),
       ?ENUM(sys),
       ?ENUM(exit),
       ?ENUM(yield)
     }.

%% not real opcodes, they are expanded like macros
synthetic_opcodes() ->
    #{
       '1+'  => [{const,1},'+'],
       '1-'  => [{const,1},'-'],
       '<'   => ['-', '0<'],
       '>'   => [swap, '-', '0<'],
       '<='  => ['-', '0<='],
       '>='  => [swap,'-', '0<='],
       '='   => ['-', '0='],
       '<>'  => ['-', 'not'],
       'u>'  => [swap, 'u<'],
       'u>=' => [swap, 'u<='],
       '0<>' => ['0=', 'not'],
       '0>'  => [{const,0},'>'],
       'setbit' => [{const,1},swap,lshift,'or'],
       'clrbit' => [{const,1},swap,lshift,invert,'and'],
       'togglebit' => [{const,1},swap,lshift,'xor'],
       'tstbit'    => [{const,1},swap,lshift,'and'],
       'setclrbit' => [{'if',[setbit],[clrbit]}],
       'abs'       => [dup,'0<',{'if',[negate]}],
       'min'       => [over,over,'<',{'if',[drop],[swap,drop]}],
       'max'       => [over,over,'<',{'if',[swap,drop],[drop]}],
       'nip'       => [swap,drop],
       'tuck'      => [swap,over],
       '-rot'      => [rot,rot],
       '2drop'     => [drop,drop],
       '2dup'      => [over,over],
       '2*'        => [dup,'+'],
       '2/'        => [{const,1},arshift],
       'sqr'       => [dup,'*'],
       'mod'       => ['2dup','/','*','-'],
       'arshift'   => [dup,{const,32},swap,'-',
		       {const,-1},swap,lshift,
		       '-rot', rshift, 'or'],
       %% sys interface
       'param@'    => [{sys,'param@'}],
       'param!'    => [{sys,'param!'}],
       'timer_init' => [{sys,timer_init}],
       'timer_start' => [{sys,timer_start}],
       'timer_stop' => [{sys,timer_stop}],
       'timer_timeout' => [{sys,timer_timeout}],
       'timer_running' => [{sys,timer_running}],
       'input@' => [{sys,'input@'}],
       'select_timer' => [{sys,select_timer}],
       'select_input' => [{sys,select_input}],
       'emit' => [{sys,emit}],
       'key' => [{sys,key}],
       'key?' => [{sys,'key?'}],
       'now' => [{sys,now}]
     }.

casm_file(File) ->
    Ls = asm_file(File),
    lists:foreach(
      fun({I,Bin}) ->
	      io:format("// program size=~w\n",
			[byte_size(Bin)]),
	      io:format("uint8_t prog~w[] = {\n  ~s };\n",
			[I, cformat(Bin,1)])
      end, lists:zip(lists:seq(1,length(Ls)), Ls)).

cformat(<<>>,_I) ->
    [];
cformat(<<C>>,_I) ->
    [io_lib:format("0x~2.16.0b",[C])];
cformat(<<C,Cs/binary>>,I) ->
    [io_lib:format("0x~2.16.0b,",[C]),
     if I rem 12 =:= 0 -> "\n  "; true -> "" end | cformat(Cs,I+1)].
    
asm_file(File) ->
    {ok,Ls} = file:consult(File),
    [asm(L) || L <- Ls].

asm(Code) ->
    transform(
      [fun expand_synthetic/1,  %% replace if and built ins
       fun encode_const/1 ,     %% generate byte code for constants
       fun collect_blocks/1,    %% collect basic blocks
       fun resolve_labels/1,    %% calculate all offsets
       fun disperse_blocks/1,   %% inject blocks
       fun encode_opcodes/1], Code).

transform([F|Fs], Code) ->
    io:format("Code = ~w\n", [Code]),
    transform(Fs, F(Code));
transform([], Code) ->
    io:format("Code = ~w\n", [Code]),
    Code.

expand_synthetic(Code) ->
    lists:flatten(expand_synth_(Code)).

expand_synth_([{'if',Then}|Code]) ->
    L = new_label(),
    [{{jop,jmpz},L}, expand_synth_(Then), {label,L} | expand_synth_(Code)];
expand_synth_([{'if',Then,Else}|Code]) ->
    L0 = new_label(),
    L1 = new_label(),
    [{{jop,jmpz},L0}, expand_synth_(Then),{{jop,jmp},L1},
     {label,L0}, expand_synth_(Else),{label,L1} | 
     expand_synth_(Code)];
expand_synth_([{Jop,L}|Code]) when
      Jop =:= jmpz; Jop =:= jmpnz;
      Jop =:= jmpltz; Jop =:= jmplez; 
      Jop =:= jmp; Jop =:= call ->
    [{{jop,Jop},L} | expand_synth_(Code)];
expand_synth_([Op|Code]) when is_tuple(Op) ->
    [Op | expand_synth_(Code)];
expand_synth_([Op|Code]) when is_atom(Op) ->
    Map = synthetic_opcodes(),
    case maps:find(Op,Map) of
	error ->
	    [Op|expand_synth_(Code)];
	{ok,Ops} when is_list(Ops) ->
	    [expand_synth_(Ops)|expand_synth_(Code)]
    end;
expand_synth_([Ops|Code]) when is_list(Ops) ->
    [expand_synth_(Ops)|expand_synth_(Code)];
expand_synth_([]) ->
    [].
    
%%
%% Replace branch labels with offsets
%% iterate until all labels are resolved
%%
resolve_labels(Code) ->
    io:format("RESOLVE = ~w\n", [Code]),
    {AddrMap,Code1} = map_labels(Code),
    case resolve_3_addr(Code1,AddrMap) of
	{true,Code2} ->
	    resolve_labels(Code2);
	{false,Code2} ->
	    case resolve_8_addr(Code2,AddrMap) of
		{true,Code3} ->
		    resolve_labels(Code3);
		{false,Code3} ->
		    case resolve_16_addr(Code3,AddrMap) of
			{true,Code4} ->
			    resolve_labels(Code4);
			{false,Code4} ->
			    Code5 = resolve_32_addr(Code4),
			    io:format("RESOLVE FAR = ~w\n", [Code5]),
			    {AddrMap1,Code6} = map_labels(Code5),
			    resolve_labels_(Code6, [], AddrMap1, 0)
		    end
	    end
    end.

%%
%% Set the correct offsets when all jumps are determined
%%
resolve_labels_([{{jop,jmpi,K},Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    Addr1 = Addr + (1+K+K*N),
    OffsetList = [ begin [Target] = maps:get(L,Map),
			 Target - Addr1
		   end || L <- Ls ],
    resolve_labels_(Code, [{{jop,jmpi,K},OffsetList}|Acc], Map, Addr1);
resolve_labels_([{{jop,JOP,K},L}|Code], Acc, Map, Addr) ->
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr+K+1),
    resolve_labels_(Code, [{{jop,JOP,K},Offset}|Acc], Map, Addr+K+1);
resolve_labels_([{label,_L}|Code], Acc, Map, Addr) ->
    %% drop label, it is not used any more
    resolve_labels_(Code, Acc, Map, Addr);
resolve_labels_([Op={block,N,_Block}|Code], Acc, Map, Addr) ->
    resolve_labels_(Code, [Op|Acc], Map, Addr+N);
resolve_labels_([], Acc, _Map, _Addr) ->
    lists:reverse(Acc).

%%
%% Locate guaranteed tiny jumps
%%
resolve_3_addr(Code,Map) ->
    resolve_3_addr_(Code, [], Map, [0], false).

resolve_3_addr_([Op={jmpi,Ls}|Code],Acc,Map,Addr,Res) ->
    N = length(Ls),
    Addr1 = add_addr([1+1+N,1+2+2*N,1+4+4*N], Addr),
    resolve_3_addr_(Code,[Op|Acc],Map,Addr1,Res);
resolve_3_addr_([Op={{jop,Jop},L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([1,2,3,5], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_3_addr(Offset) of
	true ->
	    resolve_3_addr_(Code, [{{jop,Jop,0},L}|Acc],Map,
			    add_addr(1,Addr),true);
	false ->
	    resolve_3_addr_(Code, [Op|Acc],Map,Addr1,Res)
    end;
resolve_3_addr_([Op={block,N,_Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_3_addr_(Code, [Op|Acc], LabelMap, add_addr(N,Addr),Res);
resolve_3_addr_([Op={label,_}|Code], Acc, Map, Addr, Res) ->
    resolve_3_addr_(Code, [Op|Acc], Map, Addr, Res);
resolve_3_addr_([Op|Code],Acc,Map,Addr,Res) ->
    Len = opcode_length(Op),
    resolve_3_addr_(Code, [Op|Acc], Map, add_addr(Len, Addr),Res);
resolve_3_addr_([], Acc, _LabelMap, _Addr,Res) ->
    {Res,lists:reverse(Acc)}.

%%
%% Locate guaranteed near (8-bit) jumps
%%
resolve_8_addr(Code,Map) ->
    resolve_8_addr_(Code, [], Map, [0], false).

resolve_8_addr_([Op={{jop,Jop},L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3,5], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    case is_8_addr(Offset) of
	true ->
	    resolve_8_addr_(Code, [{{jop,Jop,1},L}|Acc],Map,
			    add_addr(2,Addr),true);
	false ->
	    resolve_8_addr_(Code, [Op|Acc],Map,Addr1,Res)
    end;
resolve_8_addr_([Op={jmpi,Ls}|Code],Acc,Map,Addr,Res) ->
    N = length(Ls),
    if 
	N > 65535 ->
	    resolve_8_addr_(Code, [{{jmpi,4},Ls}|Acc],Map,
			    add_addr(1+4+4*N,Addr),true);
	N > 255 ->
	    resolve_8_addr_(Code, [{{jmpi,2},Ls}|Acc],Map,
			    add_addr(1+2+2*N,Addr),true);
	true ->
	    Addr1 = add_addr([1+1+N,1+2+2*N,1+4+4*N], Addr),
	    case lists:all(fun(X) -> X end,
			   [ begin
				 Target = maps:get(L, Map),
				 Offset = sub_addr(Target, Addr1),
				 is_8_addr(Offset)
			     end || L <- Ls ]) of
		true ->
		    resolve_8_addr_(Code, [{{jmpi,1},Ls}|Acc],Map,
				    add_addr(1+1+N,Addr),true);
		false ->
		    resolve_8_addr_(Code, [Op|Acc],Map,Addr1,Res)
	    end
    end;
resolve_8_addr_([Op={block,N,_Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_8_addr_(Code, [Op|Acc], LabelMap, add_addr(N,Addr),Res);
resolve_8_addr_([Op={label,_}|Code],Acc, LabelMap, Addr, Res) ->
    resolve_8_addr_(Code, [Op|Acc], LabelMap, Addr, Res);
resolve_8_addr_([Op|Code],Acc,Map,Addr,Res) ->
    Len = opcode_length(Op),
    resolve_8_addr_(Code, [Op|Acc], Map, add_addr(Len, Addr),Res);
resolve_8_addr_([], Acc, _Map, _Addr,Res) ->
    {Res,lists:reverse(Acc)}.


%%
%% Locate guaranteed near (16-bit) jumps
%%
resolve_16_addr(Code,Map) ->
    resolve_16_addr_(Code, [], Map, [0], false).

resolve_16_addr_([Op={{jop,Jop},L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([2,3,5], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    case is_16_addr(Offset) of
	true ->
	    resolve_16_addr_(Code, [{{jop,Jop,2},L}|Acc],Map,
			     add_addr(2,Addr),true);
	false ->
	    resolve_16_addr_(Code, [Op|Acc],Map,Addr1,Res)
    end;
resolve_16_addr_([Op={jmpi,Ls}|Code],Acc,Map,Addr,Res) ->
    N = length(Ls),
    if 
	N > 65535 ->
	    resolve_16_addr_(Code, [{{jmpi,4},Ls}|Acc],Map,
			    add_addr(1+4+4*N,Addr),true);
	N > 255 ->
	    resolve_16_addr_(Code, [{{jmpi,2},Ls}|Acc],Map,
			    add_addr(1+2+2*N,Addr),true);
	true ->
	    Addr1 = add_addr([1+1+N,1+2+2*N,1+4+4*N], Addr),
	    case lists:all(fun(X) -> X end,
			   [ begin
				 Target = maps:get(L, Map),
				 Offset = sub_addr(Target, Addr1),
				 is_16_addr(Offset)
			     end || L <- Ls ]) of
		true ->
		    resolve_16_addr_(Code, [{{jmpi,1},Ls}|Acc],Map,
				     add_addr(1+1+N,Addr),true);
		false ->
		    resolve_16_addr_(Code, [Op|Acc],Map,Addr1,Res)
	    end
    end;
resolve_16_addr_([Op={block,N,_Block}|Code], Acc, LabelMap, Addr,Res) ->
    resolve_16_addr_(Code,[Op|Acc], LabelMap, add_addr(N,Addr),Res);
resolve_16_addr_([Op={label,_}|Code], Acc, LabelMap, Addr, Res) ->
    resolve_16_addr_(Code,[Op|Acc], LabelMap, Addr, Res);
resolve_16_addr_([Op|Code],Acc,Map,Addr,Res) ->
    Len = opcode_length(Op),
    resolve_16_addr_(Code,[Op|Acc], Map, add_addr(Len, Addr),Res);
resolve_16_addr_([], Acc, _Map, _Addr,Res) ->
    {Res,lists:reverse(Acc)}.

%% replace all branch/zbranch/ibranch with far offset version

resolve_32_addr([{jmpi,Ls}|Code]) ->
    [{{jmpi,4},Ls}|resolve_32_addr(Code)];
resolve_32_addr([{{jop,Jop},L}|Code]) ->
    [{{jop,Jop,4},L}|resolve_32_addr(Code)];
resolve_32_addr([Op|Code]) ->
    [Op|resolve_32_addr(Code)];
resolve_32_addr([]) ->
    [].

%%
%% Calculate address table map and remove labels
%%
map_labels(Code) ->
    map_labels_(Code, [], #{}, [0]).

map_labels_([Op={label,L}|Code], Acc, Map, Addr) ->
    case maps:find(L, Map) of
	error -> 
	    map_labels_(Code, [Op|Acc], Map#{ L => Addr }, Addr);
	{ok,_} ->
	    erlang:error({label_exist,L})
    end;
map_labels_([Op={block,N,_Block}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(N,Addr));
map_labels_([Op={{jop,_Jop,K},_}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr(K+1,Addr));
map_labels_([Op={{jmpi,K},Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    %% assert length(N) < (1 bsl K*8)
    map_labels_(Code, [Op|Acc], Map, add_addr(1+K+K*N,Addr));
map_labels_([Op={{jop,_Jop},_L}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr([2,3,5],Addr));
map_labels_([Op={jmpi,Ls}|Code], Acc, Map, Addr) ->
    N = length(Ls),
    map_labels_(Code, [Op|Acc], Map, add_addr([1+1+N,1+2+2*N,1+4+4*N],Addr));
map_labels_([], Acc, Map, _Addr) ->
    {Map, lists:reverse(Acc)}.

%% list of alternative addresses
add_addr(A, B) when is_list(A), is_list(B) ->
    lists:usort([ X + Y || X <- A, Y <- B ]);
add_addr(A, B) when is_list(A), is_integer(B) ->
    [ X + B || X <- A];
add_addr(A, B) when is_integer(A), is_list(B) ->
    [ A + Y || Y <- B];
add_addr(A, B) when is_integer(A), is_integer(B) ->
    [ A + B ].

sub_addr(A, B) when is_list(A), is_list(B) ->
    lists:usort([ X - Y || X <- A, Y <- B ]);
sub_addr(A, B) when is_list(A), is_integer(B) ->
    [ X - B || X <- A];
sub_addr(A, B) when is_integer(A), is_list(B) ->
    [ A - Y || Y <- B];
sub_addr(A, B) when is_integer(A), is_integer(B) ->
    [ A - B ].

is_3_addr([A|_]) when A < -4; A > 3 -> false;
is_3_addr([_|As]) -> is_3_addr(As);
is_3_addr([]) -> true.

is_8_addr([A|_]) when A < -16#80; A > 16#7f -> false;
is_8_addr([_|As]) -> is_8_addr(As);
is_8_addr([]) -> true.

is_16_addr([A|_]) when A < -16#8000; A > 16#7fff -> false;
is_16_addr([_|As]) -> is_16_addr(As);
is_16_addr([]) -> true.

%%
%% Collect code blocks
%% Fixme: move combine opcodes?
%%        move optimisation of jmpz?
%%
collect_blocks(Code) ->
    collect_blocks_(Code, [], [], 0).

collect_blocks_(['0=',{{jop,jmpz},L}|Code], Block, Acc, Z) ->
    %% (!X=0) == (X != 0)
    collect_blocks_([{{jop,jmpnz},L}|Code], Block, Acc, Z);
collect_blocks_(['0<=',{{jop,jmpz},L}|Code], Block, Acc, Z) ->
    %% (!X<=0) == (X>0)
    collect_blocks_([{{jop,jmpgtz},L}|Code], Block, Acc,Z);
collect_blocks_(['0<',{{jop,jmpz},L}|Code], Block, Acc,Z) ->
    %% (!X<0) == (X>=0)
    collect_blocks_([{{jop,jmpgez},L}|Code], Block, Acc,Z);
collect_blocks_([Op={{jop,_Jop},_L}|Code], Block, Acc,Z) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)],Z);
collect_blocks_([Op={jmpi,_Ls}|Code], Block, Acc,Z) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)],Z);
collect_blocks_([Op={label,_L}|Code], Block, Acc,Z) ->
    collect_blocks_(Code, [], [Op | add_block(Block,Acc)],Z);
collect_blocks_([Op1|Code1=[Op2|Code2]], Block, Acc,Z) 
  when is_atom(Op1),is_atom(Op2) ->
    Map = opcodes(),
    N1 = maps:get(Op1, Map),
    N2 = maps:get(Op2, Map),
    if N1 < 8, N2 < 8 ->
	    collect_blocks_(Code2,[{Op1,Op2}|Block], Acc,Z+1);
       true ->
	    collect_blocks_(Code1, [Op1|Block], Acc,Z)
    end;
collect_blocks_([Opcode|Code], Block, Acc, Z) ->
    collect_blocks_(Code, [Opcode|Block], Acc, Z);
collect_blocks_([], Block, Acc, Z) ->
    io:format("compressed ~w bytes\n", [Z]),
    lists:reverse(add_block(Block,Acc)).

add_block([],Code) -> Code;
add_block(Block,Code) ->
    Basic = lists:reverse(Block),
    N = block_length(Basic),
    [{block,N,Basic}|Code].

%%
%% flatten blocks and generate instruction list
%%
disperse_blocks(Code) ->
    disperse_blocks_(Code, []).

disperse_blocks_([{block,_N,Block}|Code], Acc) ->
    disperse_blocks_(Code, [Block|Acc]);
disperse_blocks_([Op|Code], Acc) ->
    disperse_blocks_(Code, [Op|Acc]);
disperse_blocks_([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

%%
%% Encode integer constants 0, 1 encode to instructions
%% while other constants encode into 2,3 or 5 byte instructions
%%    
encode_const(Code) ->
    encode_const_(Code,[]).

encode_const_([{const,I}|Code],Acc) when is_integer(I) ->
    if I >= -4, I =< 3 ->
	    <<H0:3>> = <<I:3>>,
	    encode_const_(Code, [{literal,0,H0}|Acc]);
       I >= -16#80, I =< 16#7f ->
	    <<B0>> = <<I:8>>,
	    encode_const_(Code, [{literal,1,[B0]}|Acc]);
       I >= -16#8000, I =< 16#7fff ->
	    <<B1,B0>> = <<I:16>>,
	    encode_const_(Code, [{literal,2,[B1,B0]}|Acc]);
       I >= -16#80000000, I =< 16#7fffffff ->
	    <<B3,B2,B1,B0>> = <<I:32>>,
	    encode_const_(Code, [{literal,4,[B3,B2,B1,B0]}|Acc]);
       true ->
	    erlang:error(integer_to_big)
    end;
encode_const_([{const,true}|Code],Acc) ->
    encode_const_(Code, [{literal,0,1}|Acc]);
encode_const_([{const,false}|Code],Acc) ->
    encode_const_(Code, [{literal,0,0}|Acc]);
encode_const_([{const,boolean}|Code], Acc) ->
    encode_const_([{const,?INPUT_BOOLEAN}|Code],Acc);
encode_const_([{const,analog}|Code], Acc) ->
    encode_const_([{const,?INPUT_ANALOG}|Code],Acc);
encode_const_([{const,encoder}|Code], Acc) ->
    encode_const_([{const,?INPUT_ENCODER}|Code],Acc);
encode_const_([{sys, SysOp}|Code], Acc) ->
    Sys = case SysOp of
	      'param@' -> ?SYS_PARAM_FETCH;
	      'param!' -> ?SYS_PARAM_STORE;
	      timer_init -> ?SYS_TIMER_INIT;
	      timer_start -> ?SYS_TIMER_START;
	      timer_stop -> ?SYS_TIMER_STOP;
	      timer_timeout -> ?SYS_TIMER_TIMEOUT;
	      timer_running -> ?SYS_TIMER_RUNNING;
	      'input@' -> ?SYS_INPUT_FETCH;
	      select_timer -> ?SYS_SELECT_TIMER;
	      select_input -> ?SYS_SELECT_TIMER;
	      deselect_all -> ?SYS_DESELECT_ALL;
	      emit         -> ?SYS_EMIT;
	      key          -> ?SYS_KEY;
	      'key?'       -> ?SYS_QKEY;
	      now          -> ?SYS_NOW
	  end,
    encode_const_(Code, [{sys,Sys}|Acc]);
encode_const_([C|Code],Acc) ->
    encode_const_(Code, [C|Acc]);
encode_const_([],Acc) ->
    lists:reverse(Acc).

%% 
%% Length of basic block in bytes
%%
block_length(Code) ->
    block_length(Code,0).

block_length([Op|Code],N) ->
    block_length(Code, opcode_length(Op)+N);
block_length([],N) ->
    N.

%%
%% Size of opcode
%%
opcode_length({literal,I,_Data}) -> I+1;
opcode_length({{jmpi,I},Ls}) -> 1+(I+1)+(I+1)*length(Ls);
opcode_length({{jop,_,I},_}) -> I+1;
opcode_length({sys,_}) -> 2;
opcode_length({Op1,Op2}) ->
    Map = opcodes(),
    N1 = maps:get(Op1, Map),
    N2 = maps:get(Op2, Map),
    if N1 < 8, N2 < 8 -> 1 end;
opcode_length(Op) ->
    Map = opcodes(),
    N7 = maps:get(Op, Map),
    if N7 < 32 -> 1 end.

%%
%% Encode all opcodes into bytes
%%
encode_opcodes(Code) ->
    encode_opcodes_(Code, [], maps:merge(jopcodes(),opcodes())).

encode_opcodes_([{literal,0,I4}|Code], Acc, Map) ->
    encode_opcodes_(Code,[?OPCODE2(?JOP(literal),I4)|Acc],Map);
encode_opcodes_([{literal,K,Is}|Code], Acc, Map) ->
    encode_opcodes_(Code,cat(Is,[?OPCODE1(?JOP(literal),K-1)|Acc]),Map);
encode_opcodes_([{{jop,Jmp,0},I3}|Code], Acc, Map) ->
    N = maps:get(Jmp,Map),
    if N < 8 -> true end,
    encode_opcodes_(Code,[?OPCODE2(N,I3)|Acc],Map);
encode_opcodes_([{{jop,Jmp,K},Offset}|Code], Acc, Map) ->
    N = maps:get(Jmp,Map),
    if N < 8 -> true end,
    Is = encode_integer(Offset,K),
    encode_opcodes_(Code,cat(Is,[?OPCODE1(N,K-1)|Acc]),Map);
encode_opcodes_([{{jmpi,K},OffsetList}|Code], Acc, Map) when K>0 ->
    Ns = encode_integer(length(OffsetList),K),
    Is = [encode_integer(Offset,K)||Offset <- OffsetList],
    Ls = Ns ++ lists:append(Is),
    encode_opcodes_(Code,cat(Ls,[?OPCODE1(?JOP(jmpi),K-1)|Acc]),Map);
encode_opcodes_([{sys,Sys}|Code],Acc,Map) ->
    encode_opcodes_(Code, [Sys,?OPCODE0(?OP(sys))|Acc],Map);
encode_opcodes_([{Op1,Op2}|Code],Acc,Map) ->
    N1 = maps:get(Op1,Map),
    N2 = maps:get(Op2,Map),
    if N1 < 8, N2 < 8 -> true end,
    encode_opcodes_(Code,[?OPCODE3(N1,N2)|Acc],Map);
encode_opcodes_([Op|Code], Acc, Map) ->
    N = maps:get(Op,Map),
    if N < 32 -> true end,
    encode_opcodes_(Code,[?OPCODE0(N)|Acc],Map);
encode_opcodes_([], Acc, _Map) ->
    list_to_binary(lists:reverse(Acc)).

%% encode offset of K bytes as byte list
encode_integer(Offset, K) ->
    binary_to_list(<<Offset:K/unit:8>>).

%% cat a list 
cat([I|Is], Acc) ->
    cat(Is, [I|Acc]);
cat([], Acc) ->
    Acc.

new_label() ->
    case get(next_label) of
	undefined ->
	    put(next_label, 1),
	    0;
	I ->
	    put(next_label, I+1),
	    I
    end.

effect_all() ->
    Ls = 
	[ begin Is=[A,B],
		{Is,effect_(Is)}
	  end || A <- [dup,rot,over,drop,swap,'-','+','*'],
		 B <- [dup,rot,over,drop,swap,'-','+','*']],
    effect_filter(Ls).

effect_filter(Ls) ->
    effect_filter(Ls, sets:new()).

effect_filter([{Is,{S,S}}|Tail], Set) ->
    io:format("~w = id\n", [Is]),
    effect_filter(Tail,Set);
effect_filter([{Is,{S1,S2}}|Tail], Set) ->
    case sets:is_element({S1,S2},Set) of
	true ->
	    io:format("~w ( multiple ) ", [Is]),
	    print_stack_effect(S1,S2),
	    effect_filter(Tail,Set);
	false ->
	    io:format("~w : ", [Is]),
	    print_stack_effect(S1,S2),
	    effect_filter(Tail,sets:add_element({S1,S2},Set))
    end;
effect_filter([],_) ->
    ok.

effect(Is) ->
    {Stack,Stack2} = effect_(Is),
    print_stack_effect(Stack, Stack2),
    Stack2.

print_stack_effect(Before,After) ->
    io:format("( ~w -- ~w )\n", [lists:reverse(Before),
				 lists:reverse(After)]).

effect_(Is) ->
    {N,_,_} = minmax_depth(Is),
    Stack = generate_stack(-N),
    Stack2 = exec(Is, Stack),
    {Stack, Stack2}.


generate_stack(N) ->
    generate_stack($a,N,[]).
    
generate_stack(_C,0,Acc) ->
    Acc;
generate_stack(C, I,Acc) when I > 0 ->
    generate_stack(C+1,I-1,[list_to_atom([C]) | Acc]).


exec([I|Is],Stack) ->
    exec(Is,exec_(I,Stack));
exec([],Stack) ->
    Stack.

exec_(dup, [A|Xs])     -> [A,A|Xs];
exec_(rot, [C,B,A|Xs]) -> [A,C,B|Xs];
exec_(over, [B,A|Xs])  -> [A,B,A|Xs];
exec_(drop, [_A|Xs])   -> Xs;
exec_(swap, [B,A|Xs])  -> [A,B|Xs];
exec_('-',[A,A|Xs])    -> [{const,0}|Xs];
exec_('-',[B,A|Xs])    -> [{'-',A,B}|Xs];
exec_('+',[B,A|Xs])    -> [{'+',A,B}|Xs];
exec_('*',[B,A|Xs])    -> [{'*',A,B}|Xs];
exec_('=',[A,A|Xs])    -> [{const,1}|Xs];
exec_('=',[B,A|Xs])    -> [{'=',A,B}|Xs];
exec_('and',[A,A|Xs])  -> [A|Xs];
exec_('and',[B,A|Xs])  -> [{'and',A,B}|Xs];
exec_('or',[A,A|Xs])   -> [A|Xs];
exec_('or',[B,A|Xs])   -> [{'or',A,B}|Xs];
exec_('xor',[A,A|Xs])   -> [{const,0}|Xs];
exec_('xor',[B,A|Xs])   -> [{'xor',A,B}|Xs];
exec_('0=',[A|Xs])     -> [{'0=',A}|Xs];
exec_('0<',[A|Xs])     -> [{'0<',A}|Xs];
exec_('not',[A|Xs])    -> [{'not',A}|Xs];
exec_('/',[B,A|Xs])   -> [{'/',A,B}|Xs];
exec_('negate',[A|Xs])    -> [{'negate',A}|Xs];
exec_('invert',[A|Xs])    -> [{'invert',A}|Xs];
exec_('lshift',[B,A|Xs])    -> [{'<<',A,B}|Xs];
exec_('rshift',[B,A|Xs])    -> [{'>>',A,B}|Xs];
exec_(nop, Xs)         -> Xs;
exec_({const,C},Xs) -> [C|Xs].


%% depth calculate stack effect depth
%% depth(Instruction) -> {Min depth before,  Min depth after}
%%
minmax_depth(Is) ->
    Min0 = 3*length(Is),
    minmax_depth(Is, Min0, 0, 0).

minmax_depth([I|Is], Min, Max, Level) ->
    {Before,After} = min_depth_(I),
    Effect = After - Before,
    LevelBefore = Level - Before,
    LevelAfter = Level - After,
    Level1 = Level + Effect,
    minmax_depth(Is, 
		 erlang:min(Min,LevelBefore),
		 erlang:max(Max,LevelAfter),
		 Level1);
minmax_depth([], Min, Max, Depth) ->
    {Min,Max,Depth}.

min_depth_(dup)   -> {1,2};
min_depth_(rot)   -> {3,3};
min_depth_(over)  -> {2,3};
min_depth_(drop)  -> {1,0};
min_depth_(swap)  -> {2,2};
min_depth_('-')   -> {2,1};
min_depth_('+')   -> {2,1};
min_depth_('*')   -> {2,1};
min_depth_('=')   -> {2,1};
min_depth_('and') -> {2,1};
min_depth_('or')  -> {2,1};
min_depth_('0=')  -> {1,1};
min_depth_('0<')  -> {1,1};
min_depth_('not') -> {1,1};
min_depth_('/')   -> {2,1};
min_depth_('xor')  -> {2,1};
min_depth_('negate') -> {1,1};
min_depth_('invert') -> {1,1};
min_depth_('lshift')  -> {2,1};
min_depth_('rshift')  -> {2,1};
min_depth_('1+') -> {1,1};
min_depth_('1-') -> {1,1};
min_depth_('u<')  -> {2,1};
min_depth_('<')  -> {2,1};
min_depth_('!')  -> {2,0};
min_depth_('@')  -> {1,1};
min_depth_('nop') -> {0,0};
min_depth_('<=') -> {2,1};
min_depth_('u<=') -> {2,1};
min_depth_('ret') -> {0,0};
min_depth_({const,_C}) -> {0,1}.
