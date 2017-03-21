%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Compile & Assemble chine code
%%% @end
%%% Created : 25 Dec 2016 by Tony Rogvall <tony@rogvall.se>

-module(chine).

-export([start/0, start/1]).

-compile(export_all).
-export([effect/1, minmax_depth/1]).
-export([print_stack_effect/2]).
-export([opcodes/0, syscalls/0]).

-include("../include/chine.hrl").

options() ->
    [ {format,$f,"format", {atom,binary}, "output file format"},
      {debug,$d,"debug", undefined, "show debug information"},
      {execute,$x,"execute", undefined, "execute compile code"},
      {output,$o,"output-file", string, "output file name"},
      {version,$v,"version", undefined, "application version"},
      {help,$h,"help", undefined, "this help"}
    ].

start() ->
    start([]).

start(Args) ->
    application:load(chine),
    case getopt:parse(options(), Args) of
	{ok,{Opts,[]}} ->
	    case lists:member(help,Opts) of
		true ->
		    getopt:usage(options(), "chine"),
		    halt(0);
		false ->
		    io:format("~s: missing input file\n", ["chine"]),
		    halt(1)
	    end;
	{ok,{Opts,[File]}} ->
	    do_input(File, Opts);
	{error,Error} ->
	    io:format("~s\n",
		      [getopt:format_error(options(),Error)]),
	    getopt:usage(options(), "chine"),
	    halt(1)
    end.

do_input(File, Opts) ->
    case file:consult(File) of
	{ok,Ls0} ->
	    Ls = lists:flatten(Ls0),
	    try asm_list(Ls,Opts) of
		AsmResult ->
		    do_emit(AsmResult, Opts)
	    catch
		error:Reason ->
		    io:format("~s:error: ~p\n~p", 
			      ["chin_compile", Reason,
			       erlang:get_stacktrace()
			      ]),
		    halt(1)
	    end;
	{error,Reason} ->
	    io:format("~s:error: ~p\n", 
		      ["chin_compile", Reason]),
	    halt(1)
    end.

do_emit({Bin,Symbols,Labels}, Opts) ->
    Format = proplists:get_value(format,Opts,binary),
    SymTab = symbol_table(Symbols,Labels),
    %% io:format("SymTab = ~p\n", [SymTab]),
    Output = 
	case Format of
	    binary ->
		SymbolEntries =
		    [begin
			 BinValue = encode_symbol_value(Value),
			 LenValue = byte_size(BinValue),
			 [length(Sym),Sym,LenValue,BinValue]
		     end || {Sym,Value} <- SymTab],
		SymbolTableBin = list_to_binary(SymbolEntries),
		{File0,_Length} = file_sections(SymbolTableBin,0,Bin),
		CRC = erlang:crc32(File0),
		%% io:format("CRC = ~w, Len=~w\n", [CRC,Length]),
		{File1,_} = file_sections(SymbolTableBin,CRC,Bin),
		File1;
	    c ->
		[[begin
		   ["#define SYM_",Sym," ",integer_to_list(Value),"\n"]
		  end || {Sym,Value} <- SymTab],
		 io_lib:format("// program size=~w, sha=~s\n",
			       [byte_size(Bin),hex(crypto:hash(sha,Bin))]),
		 io_lib:format("unsigned char prog[] = {\n  ~s };\n",
			       [cformat(Bin,1)])]
	end,
    case proplists:get_value(output, Opts) of
	undefined ->
	    file:write(user,Output),
	    halt(0);
	File ->
	    case file:write_file(File, Output) of
		ok ->
		    case lists:member(execute, Opts) of
			true ->
			    Exec = filename:join([code:lib_dir(chine),
						  "bin","chine_exec"]),
			    Res = os:cmd(Exec ++ " " ++ File),
			    io:format("~s", [Res]),
			    halt(0);
			false ->
			    halt(0)
		    end;
		{error,Reason} ->
		    io:format("~s:error: ~p\n", 
			      ["chin", Reason]),
		    halt(1)
	    end
    end.


cformat(<<>>,_I) ->
    [];
cformat(<<C>>,_I) ->
    [io_lib:format("0x~2.16.0b",[C])];
cformat(<<C,Cs/binary>>,I) ->
    [io_lib:format("0x~2.16.0b,",[C]),
     if I rem 12 =:= 0 -> "\n  "; true -> "" end | cformat(Cs,I+1)].

hex(Bin) ->
    [ element(I+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}) 
      || <<I:4>> <= Bin].

file_sections(SymbolTable,CRC,Content) ->
    SymbolTableLen = byte_size(SymbolTable),
    ContentLen = byte_size(Content),
    Length = 4 + 4 + SymbolTableLen + 4 + 4 + ContentLen,
    {[$C,$H,$I,$N,
     <<?FILE_VERSION:32>>,
     <<CRC:32>>,
     <<Length:32>>,
     $S,$Y,$M,$B,
     <<SymbolTableLen:32>>,
     SymbolTable,
     $C,$O,$D,$E,
     <<ContentLen:32>>,
     Content], Length}.

symbol_table(Symbols, Labels) ->
    %% io:format("Symbols = ~p, labels = ~p\n",[Symbols, Labels]),
    lists:foldl(
      fun(L, Acc) ->
	      case maps:find(L, Labels) of
		  error ->
		      io:format("warning: exported label ~s not found\n", [L]),
		      Acc;
		  {ok,[Addr]} ->
		      [{encode_symbol_name(L), Addr}|Acc]
	      end
      end, [], [L || {{export,L},_} <- maps:to_list(Symbols)]).

encode_symbol_value(Value) when Value >= 0 ->
    if Value < -16#8000; Value > 16#7fff -> <<Value:32>>;
       Value < -16#80; Value > 16#7f ->  <<Value:16>>;
       true -> <<Value:8>>
    end.

encode_symbol_name(Name) ->
    binary_to_list(iolist_to_binary(Name)).

asm_list(Code,Opts) ->
    {Code1,Symbols} = expand_synthetic(Code,Opts),
    debugf(Opts, "pass: synthetic ~p\n", [Code1]),
    Code2 = encode_const(Code1,Opts),
    debugf(Opts, "pass: const ~p\n", [Code2]),
    Code3 = collect_blocks(Code2,Opts),
    debugf(Opts, "pass: collect ~p\n", [Code3]),
    {Code4,Labels} = resolve_labels(Code3,Opts),
    debugf(Opts, "pass: resolved ~p\n", [Code4]),
    Code5 = disperse_blocks(Code4,Opts),
    debugf(Opts, "pass: disperse ~p\n", [Code5]),
    Code6 = encode_opcodes(Code5,Opts),
    {Code6,Symbols,Labels}.


debugf(Opts,Fmt,As) ->
    case lists:member(debug, Opts) of
	true ->
	    io:format(Fmt, As);
	false ->
	    ok
    end.


%% on input a jump should look like {jmp,L} when L is a label
%% first pass transform all jumps into generic form {{jop,jmp},L}
%% later pass will determin the jump offset size K {{jop,jmp,K},L}
%% where K=0,1,2,4

jopcodes() ->
    #{
       ?JENUM(jmpz),
       ?JENUM(jmpnz),
       ?JENUM(next),
       ?JENUM(jmplz),
       ?JENUM(jmp),
       ?JENUM(call),
       ?JENUM(literal),
       ?JENUM(array)
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
       ?ENUM('not'),
       ?ENUM(invert),
       ?ENUM(negate),
       ?ENUM('/'),
       ?ENUM(shift),
       ?ENUM('!'),
       ?ENUM('@'),
       ?ENUM('>r'),
       ?ENUM('r>'),
       ?ENUM('r@'),
       ?ENUM(exit),
       ?ENUM(sys),
       ?ENUM(yield),
       ?ENUM('[]'),
       ?ENUM('execute')
     }.

syscalls() ->
    #{
       ?SENUM(sys_init),
       ?SENUM(sys_param_fetch),
       ?SENUM(sys_param_store),
       ?SENUM(sys_timer_init),
       ?SENUM(sys_timer_start),
       ?SENUM(sys_timer_stop),
       ?SENUM(sys_timer_timeout),
       ?SENUM(sys_timer_running),
       ?SENUM(sys_input_fetch),
       ?SENUM(sys_output_store),
       ?SENUM(sys_select_timer),
       ?SENUM(sys_deselect_timer),
       ?SENUM(sys_select_input),
       ?SENUM(sys_deselect_input),
       ?SENUM(sys_deselect_all),
       ?SENUM(sys_uart_send),
       ?SENUM(sys_uart_recv),
       ?SENUM(sys_uart_avail),
       ?SENUM(sys_now),
       ?SENUM(sys_gpio_input),
       ?SENUM(sys_gpio_output),
       ?SENUM(sys_gpio_set),
       ?SENUM(sys_gpio_clr),
       ?SENUM(sys_analog_send),
       ?SENUM(sys_analog_recv),
       ?SENUM(sys_can_send),
       ?SENUM(sys_terminate)
     }.

%% not real opcodes, they are expanded like macros
%% some may be expanded like calls instead?!
synthetic_opcodes() ->
    #{
       '1+'  => [{const,1},'+'],
       '1-'  => [{const,1},'-'],
       'lshift' => [shift],
       'rshift' => [negate,shift],
       '<'   => ['-', '0<'],
       '>'   => [swap, '-', '0<'],
       '<='  => ['-', '0<='],
       '>='  => [swap,'-', '0<='],
       '='   => ['-', '0='],
       '<>'  => ['-', 'not'],
       'u<'  => ['2dup','xor','0<',
		 {'if',[swap,drop,'0<'],['-','0<']}],
       'u<=' => ['2dup','xor','0<',
		 {'if',[swap,drop,'0<'],['-','0<=']}],
       'u>'  => [swap, 'u<'],
       'u>=' => [swap, 'u<='],
       '0<>' => ['0=', 'not'],
       '0>'  => [{const,0},'>'],
       '0<=' => [{const,1},'-','0<'],
       'abs' => [dup,'0<',{'if',[negate]}],
       'min' => [over,over,'<',{'if',[drop],[swap,drop]}],
       'max' => [over,over,'<',{'if',[swap,drop],[drop]}],
       'nip' => [swap,drop],
       'tuck' => [swap,over],
       '-rot' => [rot,rot],
       '2drop' => [drop,drop],
       '2dup'  => [over,over],
       '2*'    => [dup,'+'],
       'arshift'   => [dup,{const,32},swap,'-',
		       {const,-1},swap,shift,
		       '-rot', negate,shift, 'or'],
       '2/'    => [{const,1},arshift],
       'sqr'   => [dup,'*'],
       'mod'   => ['2dup','/','*','-'],
       'jmp*'      => ['>r', exit],  %% ( caddr -- )
       ';'         => [exit],

       %% utils
       'setbit' => [{const,1},swap,shift,'or'],
       'clrbit' => [{const,1},swap,shift,invert,'and'],
       'togglebit' => [{const,1},swap,shift,'xor'],
       'tstbit'    => [{const,1},swap,shift,'and'],
       'setclrbit' => [{'if',[setbit],[clrbit]}],

       %% sys interface
       'param@'    => [{sys,sys_param_fetch}],
       'param!'    => [{sys,sys_param_store}],
       'timer_init' => [{sys,sys_timer_init}],
       'timer_start' => [{sys,sys_timer_start}],
       'timer_stop' => [{sys,sys_timer_stop}],
       'timer_timeout' => [{sys,sys_timer_timeout}],
       'timer_running' => [{sys,sys_timer_running}],
       'input@'        => [{sys,sys_input_fetch}],
       'output!'       => [{sys,sys_output_store}],
       select_timer => [{sys,sys_select_timer}],
       deselect_timer => [{sys,sys_deselect_timer}],
       select_input => [{sys,sys_select_input}],
       deselect_input => [{sys,sys_deselect_input}],
       deselect_all  => [{sys,sys_deselect_all}],
       uart_send     => [{sys,sys_uart_send}],
       uart_recv     => [{sys,sys_uart_recv}],
       uart_avail    => [{sys,sys_uart_avail}],
       now           => [{sys,sys_now}],
       gpio_input    => [{sys,sys_gpio_input}],
       gpio_output   => [{sys,sys_gpio_output}],
       gpio_set      => [{sys,sys_gpio_set}],
       gpio_clr      => [{sys,sys_gpio_clr}],
       gpio_get      => [{sys,sys_gpio_get}],
       gpio_mask     => [{sys,sys_gpio_mask}],
       analog_set    => [{sys,sys_analog_set}],
       analog_clr    => [{sys,sys_analog_clr}],
       can_send      => [{sys,sys_can_send}],
       terminate     => [{sys,sys_terminate}],
       %% aliases
       'emit'        => [{sys,sys_uart_send}],
       'key'         => [{sys,sys_uart_recv}],
       '?key'        => [{sys,sys_uart_avail}]
     }.

expand_synthetic(Code,_Opts) ->
    expand_synth_(Code,[],#{}).

expand_synth_([{'if',Then}|Code],Acc,Sym) ->
    L = new_label(),
    Block = [{{jop,jmpz},L},Then,{label,L}],
    expand_synth_(Block ++ Code,Acc,Sym);
expand_synth_([{'if',Then,Else}|Code],Acc,Sym) ->
    L0 = new_label(),
    L1 = new_label(),
    Block = [{{jop,jmpz},L0},Then,{{jop,jmp},L1},
	     {label,L0},Else,{label,L1}],
     expand_synth_(Block++Code,Acc,Sym);
expand_synth_([{'again',Loop}|Code],Acc,Sym) ->
    L0 = new_label(),
    Block = [{label,L0},Loop,{{jop,jmp},L0}],
    expand_synth_(Block++Code,Acc,Sym);
expand_synth_([{'until',Loop}|Code],Acc,Sym) ->
    L0 = new_label(),
    Block = [{label,L0},Loop,{{jop,jmpz},L0}],
    expand_synth_(Block++Code,Acc,Sym);
expand_synth_([{'repeat',While,Loop}|Code],Acc,Sym) ->
    L0 = new_label(),
    L1 = new_label(),
    Block = [{label,L0},While,{{jop,jmpz},L1},
	     Loop,{{jop,jmp},L0},{label,L1}],
    expand_synth_(Block++Code,Acc,Sym);
expand_synth_([{'for',Loop}|Code],Acc,Sym) ->
    L0 = new_label(),
    Block = ['>r', {label,L0},Loop,{{jop,next},L0}],
    expand_synth_(Block++Code,Acc,Sym);
expand_synth_([{enum,Ls}|Code],Acc,Sym) ->
    Sym1 = add_enums_(Ls, 0, Sym),
    expand_synth_(Code,Acc,Sym1);
expand_synth_([{comment,_Comment}|Code],Acc,Sym) ->
    %% just a comment ignore it
    expand_synth_(Code,Acc,Sym);
expand_synth_([{Jop,L}|Code],Acc,Sym) when
      Jop =:= jmpz; Jop =:= jmpnz;
      Jop =:= next; Jop =:= jmplz;
      Jop =:= jmp; Jop =:= call ->
    L1 = normalize_label(L),
    expand_synth_(Code,[{{jop,Jop},L1}|Acc], Sym);
expand_synth_([Op={const,C}|Code],Acc,Sym) when is_integer(C) ->
    expand_synth_(Code,[Op|Acc],Sym);
expand_synth_([{const,E}|Code],Acc,Sym) ->
    case maps:find({enum,E}, Sym) of
	{ok,C} ->
	    expand_synth_(Code,[{const,C}|Acc],Sym);
	error ->
	    io:format("error: symbol ~p not defined\n", [E]),
	    expand_synth_(Code,[{const,E} | Acc],Sym)
    end;
expand_synth_([{export,L}|Code],Acc,Sym) ->
    L1 = normalize_label(L),
    Sym1 = maps:put({export,L1}, 0, Sym),
    expand_synth_(Code,Acc,Sym1);
expand_synth_([{label,L}|Code],Acc,Sym) ->
    L1 = normalize_label(L),
    expand_synth_(Code,[{label,L1}|Acc],Sym);
expand_synth_([Op|Code],Acc,Sym) when is_tuple(Op) ->
    expand_synth_(Code,[Op|Acc],Sym);
expand_synth_([Op|Code],Acc,Sym) when is_atom(Op) ->
    Map = synthetic_opcodes(),
    case maps:find(Op,Map) of
	error ->
	    expand_synth_(Code,[Op|Acc],Sym);
	{ok,Ops} when is_list(Ops) ->
	    expand_synth_(Ops++Code,Acc,Sym)
    end;
expand_synth_([Op|Code],Acc,Sym) when is_integer(Op) ->
    expand_synth_(Code,[{const,Op}|Acc],Sym);
expand_synth_([[]|Code],Acc,Sym) ->
    expand_synth_(Code,Acc,Sym);
expand_synth_([Ops|Code],Acc,Sym) when is_list(Ops) ->
    try erlang:iolist_to_binary(Ops) of
	Bin ->
	    expand_synth_(Code,[{string,binary_to_list(Bin)}|Acc],Sym)
    catch
	error:_ ->
	    expand_synth_(Ops++Code,Acc,Sym)
    end;
expand_synth_([],Acc,Sym) ->
    {lists:reverse(Acc),Sym}.

add_enums_([E|Es], I, Sym) ->
    Sym1 = maps:put({enum,E}, I, Sym),
    add_enums_(Es, I+1, Sym1);
add_enums_([], _I, Sym) ->
    Sym.

normalize_label(L) when is_atom(L) ->    
    atom_to_list(L);
normalize_label(L) when is_list(L) ->
    try iolist_size(L) of
	Len when Len < 256 -> L;
	_ ->
	    io:format("label name too long ~s\n", [L]),
	    erlang:error({label_too_loong, L})
    catch
	error:_ ->
	    io:format("label name not string ~p\n", [L]),
	    erlang:error({label_not_string, L})
    end.

%%
%% Replace branch labels with offsets
%% iterate until all labels are resolved
%%
resolve_labels(Code,Opts) ->
    debugf(Opts,"RESOLVE = ~w\n", [Code]),
    {AddrMap,Code1} = map_labels(Code),
    case resolve_3_addr(Code1,AddrMap) of
	{true,Code2} ->
	    resolve_labels(Code2,Opts);
	{false,Code2} ->
	    case resolve_8_addr(Code2,AddrMap) of
		{true,Code3} ->
		    resolve_labels(Code3,Opts);
		{false,Code3} ->
		    case resolve_16_addr(Code3,AddrMap) of
			{true,Code4} ->
			    resolve_labels(Code4,Opts);
			{false,Code4} ->
			    Code5 = resolve_32_addr(Code4),
			    debugf(Opts, "RESOLVE FAR = ~w\n", [Code5]),
			    {AddrMap1,Code6} = map_labels(Code5),
			    resolve_labels_(Code6, [], AddrMap1, 0)
		    end
	    end
    end.

%%
%% Set the correct offsets when all jumps are determined
%%
resolve_labels_([{{jop,JOP,Kv},L}|Code], Acc, Map, Addr) ->
    K = variant_length(Kv),
    [Target] = maps:get(L, Map),
    Offset = Target - (Addr+K+1),
    resolve_labels_(Code, [{{jop,JOP,Kv},Offset}|Acc], Map, Addr+K+1);
resolve_labels_([{label,_L}|Code], Acc, Map, Addr) ->
    %% drop label, it is not used any more
    resolve_labels_(Code, Acc, Map, Addr);
resolve_labels_([{block,N,Block}|Code], Acc, Map, Addr) ->
    Block1 = resolve_caddr(Block, [], Map),
    resolve_labels_(Code, [{block,N,Block1}|Acc], Map, Addr+N);
resolve_labels_([], Acc, Map, _Addr) ->
    {lists:reverse(Acc), Map}.

%% resolve absolute addresses
resolve_caddr([{caddr,K,L}|Code], Acc, Map) ->
    [Target] = maps:get(L, Map),
    resolve_caddr(Code, [{literal,K,Target}|Acc], Map);

resolve_caddr([{array,Ka,Es}|Code], Acc, Map) ->
    Es1 = [ case E of
		{literal,_,_} -> E;
		{caddr,K,L} ->
		    [Target] = maps:get(L, Map),
		    {literal,K,Target}
	    end || E <- Es],
    resolve_caddr(Code, [{array,Ka,Es1}|Acc], Map);
resolve_caddr([Op|Code], Acc, Map) ->
    resolve_caddr(Code, [Op|Acc], Map);
resolve_caddr([], Acc, _Map) ->
    lists:reverse(Acc).

%%
%% Locate guaranteed tiny jumps
%%
resolve_3_addr(Code,Map) ->
    resolve_3_addr_(Code, [], Map, [0], false).

resolve_3_addr_([Op={{jop,Jop},L}|Code],Acc,Map,Addr,Res) ->
    Addr1 = add_addr([1,2,3,5], Addr),
    Target = maps:get(L, Map),
    Offset = sub_addr(Target, Addr1),
    %% if all offsets are short then we select short
    case is_3_addr(Offset) of
	true ->
	    resolve_3_addr_(Code, [{{jop,Jop,int3},L}|Acc],Map,
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
	    resolve_8_addr_(Code, [{{jop,Jop,int8},L}|Acc],Map,
			    add_addr(2,Addr),true);
	false ->
	    resolve_8_addr_(Code, [Op|Acc],Map,Addr1,Res)
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
	    resolve_16_addr_(Code, [{{jop,Jop,int16},L}|Acc],Map,
			     add_addr(2,Addr),true);
	false ->
	    resolve_16_addr_(Code, [Op|Acc],Map,Addr1,Res)
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
resolve_32_addr([{{jop,Jop},L}|Code]) ->
    [{{jop,Jop,int32},L}|resolve_32_addr(Code)];
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
map_labels_([Op={{jop,_Jop,Kv},_}|Code], Acc, Map, Addr) ->
    K = variant_length(Kv),
    map_labels_(Code, [Op|Acc], Map, add_addr(K+1,Addr));
map_labels_([Op={{jop,_Jop},_L}|Code], Acc, Map, Addr) ->
    map_labels_(Code, [Op|Acc], Map, add_addr([2,3,5],Addr));
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
collect_blocks(Code,Opts) ->
    {Code1,Z} = collect_blocks_(Code, [], [], 0),
    debugf(Opts,"compressed ~w bytes\n", [Z]),
    Code1.

collect_blocks_(['0=',{{jop,jmpz},L}|Code], Block, Acc, Z) ->
    %% (!X=0) == (X != 0)
    collect_blocks_([{{jop,jmpnz},L}|Code], Block, Acc, Z);
collect_blocks_([Op={{jop,_Jop},_L}|Code], Block, Acc,Z) ->
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
    {lists:reverse(add_block(Block,Acc)),Z}.


add_block([],Code) -> Code;
add_block(Block,Code) ->
    Basic = lists:reverse(Block),
    N = block_length(Basic),
    [{block,N,Basic}|Code].

%%
%% flatten blocks and generate instruction list
%%
disperse_blocks(Code,_Opts) ->
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
encode_const(Code,_Opts) ->
    encode_const_(Code,[]).

encode_const_([{const,C}|Code], Acc) ->
    L = encode_literal(C),
    encode_const_(Code, [L|Acc]);
encode_const_([{caddr,L}|Code], Acc) ->
    %% caddr is an offset from program start, can only 
    %% be calculated when all labels have been calculated
    %% but is now assumed to fit in a 16 bit integer FIXME
    encode_const_(Code, [{caddr,uint16,L}|Acc]);
encode_const_([{string,S}|Code], Acc) when is_list(S) ->
    encode_const_([{array,[{const,C} || C <- S]}|Code], Acc);
encode_const_([{string,S}|Code], Acc) when is_binary(S) ->
    encode_const_([{array,[{const,C} || <<C>> <= S]}|Code], Acc);
encode_const_([{array,Es}|Code], Acc) ->
    N = length(Es),
    Es1 = [case E of
	       {const,C} -> encode_literal(C);
	       {caddr,L} -> {caddr,uint16,L} %% FIXME
	   end || E <- Es],
    K = if N =:= 0 -> 1;
	   true -> lists:max([opcode_length(Op) || Op <- Es1])
	end,
    A = if N < 8, K=:=1       -> {array,uint3x8,Es1};
	   N < 8, K=:=2       -> {array,uint3x8,Es1};
	   N < 256, K=:=2     -> {array,uint8x8,Es1};
	   N < 256, K=:=3     -> {array,uint8x16,Es1};
	   N < 256, K=:=5     -> {array,uint8x32,Es1};
	   N < 65536, K =:= 2 -> {array,uint16x8,Es1};
	   N < 65536, K =:= 3 -> {array,uint16x16,Es1};
	   N < 65536, K =:= 5 -> {array,uint16x32,Es1}
	end,
    encode_const_(Code, [A|Acc]);
encode_const_([{sys, SysOp}|Code], Acc) ->
    Sys = maps:get(SysOp, syscalls()),
    encode_const_(Code, [{sys,Sys}|Acc]);
encode_const_([C|Code],Acc) ->
    encode_const_(Code, [C|Acc]);
encode_const_([],Acc) ->
    lists:reverse(Acc).

%% encode some integer constants
encode_literal(true)    -> encode_literal(1);
encode_literal(false)   -> encode_literal(0);
encode_literal(boolean) -> encode_literal(?INPUT_BOOLEAN);
encode_literal(analog)  -> encode_literal(?INPUT_ANALOG);
encode_literal(encoder) -> encode_literal(?INPUT_ENCODER);
encode_literal(I) when is_integer(I) ->
    if I >= -4, I =< 3 ->
	    {literal,int3,I};
       I >= -16#80, I =< 16#7f ->
	    {literal,int8,I};
       I >= -16#8000, I =< 16#7fff ->
	    {literal,int16,I};
       I >= -16#80000000, I =< 16#7fffffff ->
	    {literal,int32,I}
    end.
    
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
opcode_length({literal,int3,_X}) -> 1;
opcode_length({literal,int8,_X}) -> 2;
opcode_length({literal,int16,_X}) -> 3;
opcode_length({literal,int32,_X}) -> 5;
opcode_length({literal,uint3,_X}) -> 1;
opcode_length({literal,uint8,_X}) -> 2;
opcode_length({literal,uint16,_X}) -> 3;
opcode_length({literal,uint32,_X}) -> 5;
opcode_length({caddr,uint3,_L})  -> 1;
opcode_length({caddr,uint8,_L})  -> 2;
opcode_length({caddr,uint16,_L}) -> 3;
opcode_length({caddr,uint32,_L}) -> 5;

opcode_length({array,uint3x8,Ls})  -> 1+length(Ls);
opcode_length({array,uint8x8,Ls})  -> 1+1+length(Ls);
opcode_length({array,uint8x16,Ls}) -> 1+1+2*length(Ls);
opcode_length({array,uint8x32,Ls}) -> 1+1+4*length(Ls);
opcode_length({array,uint16x8,Ls}) -> 1+2+length(Ls);
opcode_length({array,uint16x16,Ls}) -> 1+2+2*length(Ls);
opcode_length({array,uint16x32,Ls}) -> 1+2+4*length(Ls);

opcode_length({{jop,_,int3},_})  -> 1;
opcode_length({{jop,_,int8},_})  -> 2;
opcode_length({{jop,_,int16},_}) -> 3;
opcode_length({{jop,_,int32},_}) -> 5;
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
encode_opcodes(Code,_Opts) ->
    encode_opcodes_(Code, [], maps:merge(jopcodes(),opcodes())).

encode_opcodes_([{literal,int3,I}|Code], Acc, Map) ->
    encode_opcodes_(Code,[?OPCODE2(?JOP(literal),I)|Acc],Map);
encode_opcodes_([{literal,uint3,I}|Code], Acc, Map) ->
    encode_opcodes_(Code,[?OPCODE2(?JOP(literal),I)|Acc],Map);
encode_opcodes_([{literal,Kv,I}|Code], Acc, Map) ->
    K = variant_length(Kv),
    Kc = variant_code(Kv),
    Is = encode_integer(I,K),
    encode_opcodes_(Code,cat(Is,[?OPCODE1(?JOP(literal),Kc)|Acc]),Map);
encode_opcodes_([{{jop,Jmp,int3},I}|Code], Acc, Map) ->
    N = maps:get(Jmp,Map),
    if N < 8 -> true end,
    encode_opcodes_(Code,[?OPCODE2(N,I)|Acc],Map);
encode_opcodes_([{{jop,Jmp,Kv},Offset}|Code], Acc, Map) ->
    K = variant_length(Kv),
    Kc = variant_code(Kv),
    N = maps:get(Jmp,Map),
    true = N < 8,
    Is = encode_integer(Offset,K),
    encode_opcodes_(Code,cat(Is,[?OPCODE1(N,Kc)|Acc]),Map);
%% ARRAY
encode_opcodes_([{array,uint3x8,Es}|Code],Acc,Map) ->
    N = length(Es),
    Is = [encode_integer(X,1) || {literal,_,X} <- Es],
    Ls = lists:append(Is),
    encode_opcodes_(Code,cat(Ls,[?OPCODE2(?JOP(array),N)|Acc]),Map);    
encode_opcodes_([{array,Kv,Es}|Code],Acc,Map) ->
    N = length(Es),
    K = variant_length(Kv),
    Kc = variant_code(Kv),
    Ke = element_length(Kv),
    Ns = encode_integer(N,K),
    Is = [encode_integer(X,Ke) || {literal,_,X} <- Es],
    Ls = Ns ++ lists:append(Is),
    encode_opcodes_(Code,cat(Ls,[?OPCODE1(?JOP(array),Kc)|Acc]),Map);
%% SYS
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

%%
%%variant_length(int3)  -> 0;
variant_length(int8)  -> 1;
variant_length(int16) -> 2;
variant_length(int32) -> 4;
variant_length(int64) -> 8;
%%variant_length(uint3)  -> 0;
variant_length(uint8)  -> 1;
variant_length(uint16) -> 2;
variant_length(uint32) -> 4;
variant_length(uint64) -> 8;
%%variant_length(uint3x8)   -> 0;
variant_length(uint8x8)   -> 1;
variant_length(uint8x16)  -> 1;
variant_length(uint8x32)  -> 1;
variant_length(uint16x8)  -> 2;
variant_length(uint16x16) -> 2;
variant_length(uint16x32) -> 2.

element_length(uint3x8)   -> 1;
element_length(uint8x8)   -> 1;
element_length(uint8x16)  -> 2;
element_length(uint8x32)  -> 4;
element_length(uint16x8)  -> 1;
element_length(uint16x16) -> 2;
element_length(uint16x32) -> 4.

variant_code(int8)      -> 0;
variant_code(int16)     -> 1;
variant_code(int32)     -> 2;
variant_code(uint8)     -> 0;
variant_code(uint16)    -> 1;
variant_code(uint32)    -> 2;
variant_code(uint8x8)   -> 0;
variant_code(uint8x16)  -> 1;
variant_code(uint8x32)  -> 2;
variant_code(uint16x8)  -> 4;
variant_code(uint16x16) -> 5;
variant_code(uint16x32) -> 6.

%% encode offset of K bytes as byte list
encode_integer(X,1) -> binary_to_list(<<X:8>>);
encode_integer(X,2) -> binary_to_list(<<X:16>>);
encode_integer(X,4) -> binary_to_list(<<X:32>>);
encode_integer(X,8) -> binary_to_list(<<X:64>>).

%% cat a list 
cat([I|Is], Acc) ->
    cat(Is, [I|Acc]);
cat([], Acc) ->
    Acc.

new_label() ->
    case get(next_label) of
	undefined ->
	    put(next_label, 1),
	    "L0";
	I ->
	    put(next_label, I+1),
	    [$L|integer_to_list(I)]
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
exec_('shift',[B,A|Xs])    -> [{'<<',A,B}|Xs];
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
min_depth_('shift')  -> {2,1};
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
