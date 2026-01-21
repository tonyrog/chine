%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Disassembler
%%% @end
%%% Created : 11 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chine_disasm).

-export([file/1]).
-export([show/1]).


-include("../include/chine.hrl").

file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    dis(Bin);
	Error ->
	    Error
    end.

show(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    {Size,CodeSections,SymTab} = dis(Bin), 
	    show_(CodeSections, SymTab),
	    {ok, Size};
	Error ->
	    Error
    end.

show_([{_Addr0,_Addr1,Content,Code}|Sections],SymTab) ->
    show__(Code, Content, SymTab),
    show_(Sections, SymTab);
show_([], _SymTab) ->
    ok.

show__([{Addr,Len,{call,{Type,Offset}}}|Code],Content,SymTab) ->
    <<Data:Len/binary, Content1/binary>> = Content,
    Target = Addr+Len+Offset,
    Op = case maps:find(Target, SymTab) of    
	     {ok, Name} -> {call, unicode:characters_to_list(Name)};
	     error -> {call,{Type,Offset}}
	 end,
    show_op(Addr, Op, Data, SymTab),
    show__(Code, Content1, SymTab);
show__([{Addr,Len,Op}|Code], Content, SymTab) ->
    <<Data:Len/binary, Content1/binary>> = Content,
    show_op(Addr, Op, Data, SymTab),
    show__(Code, Content1, SymTab);
show__([], _Content,  _) ->
    ok.

show_op(Addr, Op, Data, SymTab) ->
    case maps:find(Addr, SymTab) of
	{ok,Label} ->
	    io:format("~s:\n", [Label]);
	error -> 
	    ok
    end,
    Hex0 = format_hex(Data),
    Len = length(Hex0),
    if Len > 8 ->
	    {Hex1,HexR} = lists:split(8, Hex0),
	    io:format("~04w: ~-24s ", [Addr,Hex1]),
	    show_opcode(Op),
	    show_hex(Addr+8, HexR);
       true ->
	    io:format("~04w: ~-24s ", [Addr,Hex0]),
	    show_opcode(Op)
    end.

show_opcode(Op) ->
    case Op of
	{call,_} ->
	    io:format("~p\n", [Op]);
	_ ->
	    io:format("~w\n", [Op])
    end.

show_hex(Addr, Hex0) ->
    Len = length(Hex0),
    if Len =:= 0 ->
	    ok;
       Len > 8 ->
	    {Hex1,HexR} = lists:split(8, Hex0),
	    io:format("~04w: ~-24s\n", [Addr,Hex1]),
	    show_hex(Addr+8, HexR);
       true ->
	    io:format("~04w: ~-24s\n", [Addr,Hex0])
    end.
    
format_hex(Bin) ->
    [[hex(H),hex(L),$\s] || <<H:4,L:4>> <= Bin].

hex(X) -> element(X+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).

dis(<<"CHIN", FileVersion:32, FileCRC:32, 
      SectionsLength:32, Sections:SectionsLength/binary, _Trail/binary>>) ->
    File0 = <<"CHIN", FileVersion:32, 0:32, 
	      SectionsLength:32, Sections/binary>>,
    case erlang:crc32(File0) of
       FileCRC ->
	    Ls = [{Name,Data} || 
		     <<Name:4/binary, Len:32, Data:Len/binary>> <= Sections],
	    dis_sections(Ls);
       _ ->
	    {error, badcrc}
    end.

dis_sections(Sections) ->
    SymTab = dis_symb(Sections, #{}),
    dis_code(Sections, 0, SymTab, []).

dis_code([{<<"CODE">>, Content} | Sections], Addr, SymTab, Code0) ->
    {Addr1,Code1} = dis_opcodes(Content, Addr, SymTab, []),
    dis_code(Sections, Addr1, SymTab, [{Addr,Addr1,Content,Code1}|Code0]);
dis_code([_|Sections], Addr1, SymTab, Code1) ->
    dis_code(Sections, Addr1, SymTab, Code1);
dis_code([], Addr1, SymTab, Code) ->
    {Addr1,lists:reverse(Code),SymTab}.

dis_opcodes(<<>>, Addr, _SymTab, Code) ->
    {Addr, lists:reverse(Code)};
dis_opcodes(<<0:2, ?SYS:6, Sys:8, Content/binary>>, Addr, SymTab, Code) ->
    OpA =
	case Sys+2 of
	    #sys.sys_init -> {sys,init};
	    #sys.sys_terminate -> {sys, terminate};
	    #sys.sys_now -> {sys, now};
	    #sys.sys_emit -> {sys, emit};
	    #sys.sys_recv -> {sys, recv};
	    #sys.sys_avail -> {sys, avail};
	    #sys.sys_param_fetch -> {sys, param_fetch};
	    #sys.sys_param_store -> {sys, param_store};
	    #sys.sys_timer_init -> {sys, timer_init};
	    #sys.sys_timer_start -> {sys, timer_start};
	    #sys.sys_timer_stop -> {sys, timer_stop};
	    #sys.sys_timer_timeout -> {sys, timer_timeout};
	    #sys.sys_timer_running -> {sys, timer_running};
	    #sys.sys_input_fetch -> {sys, input_fetch};
	    #sys.sys_output_store -> {sys, output_store};
	    #sys.sys_select_timer -> {sys, select_timer};
	    #sys.sys_deselect_timer -> {sys, deselect_timer};
	    #sys.sys_select_input -> {sys, select_input};
	    #sys.sys_deselect_input -> {sys, deselect_input};
	    #sys.sys_deselect_all -> {sys, deselect_all};
	    #sys.sys_uart_connect -> {sys, uart_connect};
	    #sys.sys_uart_send -> {sys, uart_send};
	    #sys.sys_uart_recv -> {sys, uart_recv};
	    #sys.sys_uart_avail -> {sys, uart_avail};
	    #sys.sys_uart_disconnect -> {sys, uart_disconnect};
	    #sys.sys_gpio_input -> {sys, gpio_input};
	    #sys.sys_gpio_output -> {sys, gpio_output};
	    #sys.sys_gpio_set -> {sys, gpio_set};
	    #sys.sys_gpio_clr -> {sys, gpio_clr};
	    #sys.sys_gpio_get -> {sys, gpio_get};
	    #sys.sys_analog_send -> {sys, analog_send};
	    #sys.sys_analog_recv -> {sys, analog_recv};
	    #sys.sys_can_connect -> {sys, can_connect};
	    #sys.sys_can_send -> {sys, can_send};
	    #sys.sys_can_recv -> {sys, can_recv};
	    #sys.sys_can_avail -> {sys, can_avail};
	    #sys.sys_can_disconnect -> {sys, can_disconnect};
	    #sys.sys_file_open -> {sys, file_open};
	    #sys.sys_file_write -> {sys, file_write};
	    #sys.sys_file_read -> {sys, file_read};
	    #sys.sys_file_close -> {sys, file_close};
	    _ -> {sys,{unknown, Sys}}
	end,
    Len = 2,
    dis_opcodes(Content, Addr+Len, SymTab, [{Addr,Len,OpA}|Code]);    
dis_opcodes(<<0:2, OP:6, Content/binary>>, Addr, SymTab, Code) ->
    OpA = case OP+2 of
	      #opcode.dup -> dup;
	      #opcode.rot -> rot;
	      #opcode.over -> over;
	      #opcode.drop -> drop;
	      #opcode.swap -> swap;
	      #opcode.'-' -> '-';
	      #opcode.'+' -> '+';
	      #opcode.'*' -> '*';
	      %% op6
	      #opcode.'nop' -> 'nop';
	      #opcode.'and' -> 'and';
	      #opcode.'or' -> 'or';
	      #opcode.'xor' -> 'xor';
	      #opcode.'0=' -> '0=';
	      #opcode.'0<' -> '0<';
	      #opcode.'not' -> 'not';
	      #opcode.negate -> negate;
	      #opcode.'/' -> '/';
	      #opcode.shift -> shift;
	      #opcode.'!' -> '!';
	      #opcode.'@' -> '@';
	      #opcode.'>r' -> '>r';
	      #opcode.'r>' -> 'r>';
	      #opcode.'r@' -> 'r@';
	      #opcode.exit -> exit;
	      #opcode.sys -> sys;
	      #opcode.yield -> yield;
	      #opcode.'[]' -> '[]';
	      #opcode.execute -> execute;
	      #opcode.'fp@' -> 'fp@';
	      #opcode.'fp!' -> 'fp!';
	      #opcode.'sp@' -> 'sp@';
	      #opcode.'sp!' -> 'sp!';
	      #opcode.'c!' -> 'c!';
	      #opcode.'c@' -> 'c@';
	      #opcode.size -> size;
	      _ -> {op0,{unknown,OP}}
	  end,
    Len = 1,
    dis_opcodes(Content, Addr+Len, SymTab, [{Addr,Len,OpA}|Code]);
dis_opcodes(<<1:2, EE:2, ?ARRAY:4, Content/binary>>, Addr, SymTab, Code) ->
    {Op, Size, Content1} = dis_array(EE, Content),
    Len = 1+Size,
    dis_opcodes(Content1, Addr+Len, SymTab, [{Addr,Len,Op}|Code]);
dis_opcodes(<<1:2, EE:2, JOP:4, A:(1 bsl EE)/signed-unit:8, Content/binary>>,
	    Addr, SymTab, Code) ->
    Int = case EE of
	      0 -> int8;
	      1 -> int16;
	      2 -> int32;
	      3 -> int64
	  end,
    OpA = case JOP+2 of
	      #jopcode.jmpz -> {jmpz,{Int,A}};
	      #jopcode.jmpnz -> {jmpnz,{Int,A}};
	      #jopcode.next -> {next,{Int,A}};
	      #jopcode.jmplz -> {jmplz,{Int,A}};
	      #jopcode.jmp   -> {jmp,{Int,A}};
	      #jopcode.call  -> {call,{Int,A}};
	      #jopcode.literal -> {literal,{Int,A}};
	      #jopcode.arg     -> {arg,{Int,A}};
	      #jopcode.fenter  ->
		  L = A band 16#ffff,
		  N = A bsr 16,
		  {fenter,{Int,{N,L}}};
	      #jopcode.fleave  -> 
		  R = A band 16#f,
		  N = A bsr 4,
		  {fleave,{Int,{N,R}}};
	      #jopcode.fset -> 
		  {fset,{Int,A}};
	      _ -> {op1,{unknown,JOP}}
	  end,
    Len = 1+(1 bsl EE),
    dis_opcodes(Content, Addr+Len, SymTab, [{Addr,Len,OpA}|Code]);

dis_opcodes(<<2:2, A:3/signed, JOP:3, Content/binary>>, 
	    Addr, SymTab, Code) ->
    OpA = case JOP+2 of
	      #jopcode.jmpz -> {jmpz,{int3,A}};
	      #jopcode.jmpnz -> {jmpnz,{int3,A}};
	      #jopcode.next -> {next,{int3,A}};
	      #jopcode.jmplz -> {jmplz,{int3,A}};
	      #jopcode.jmp   -> {jmp,{int3,A}};
	      #jopcode.call  -> {call,{int3,A}};
	      #jopcode.literal -> {literal,{int3,A}};
	      _ -> {op2, {unknown,JOP}}
	  end,
    Len = 1,
    dis_opcodes(Content, Addr+Len, SymTab, [{Addr,Len,OpA}|Code]);

dis_opcodes(<<3:2, OP2:3, OP1:3, Content/binary>>, Addr, SymTab, Code) ->
    OpA = case OP1+2 of
	      #opcode.dup -> dup;
	      #opcode.rot -> rot;
	      #opcode.over -> over;
	      #opcode.drop -> drop;
	      #opcode.swap -> swap;
	      #opcode.'-' -> '-';
	      #opcode.'+' -> '+';
	      #opcode.'*' -> '*'
	  end,
    OpB = case OP2+2 of
	      #opcode.dup -> dup;
	      #opcode.rot -> rot;
	      #opcode.over -> over;
	      #opcode.drop -> drop;
	      #opcode.swap -> swap;
	      #opcode.'-' -> '-';
	      #opcode.'+' -> '+';
	      #opcode.'*' -> '*'
	  end,
    Len = 1,
    dis_opcodes(Content, Addr+Len, SymTab, [{Addr,Len,{OpA,OpB}}|Code]).

dis_array(0, <<Size:8, Array:Size/binary, Tail/binary>>) ->
    {dis_arr(Array), Size+1, Tail};
dis_array(1, <<Size:16, Array:Size/binary, Tail/binary>>) ->
    {dis_arr(Array), Size+1, Tail};
dis_array(2, <<Size:32, Array:Size/binary, Tail/binary>>) ->
    {dis_arr(Array), Size+1, Tail}.
%% dis_array(3, <<Size:64, Array:Size/binary, Tail/binary>>) ->
%%    {dis_arr(Array), Size+1, Tail}.

dis_arr(<<EType:8, Binary/binary>>) ->
    EEEE = EType band 16#0f,
    L = 8 bsl EEEE,
    T = if EType band 16#10 =:= 16#10 -> {float,L};
	   EType band 16#80 =:= 16#80 -> {int,L};
	   true -> {uint,L}
	end,
    {array, T, dis_array_elements(T, Binary)}.

dis_array_elements({int,Size}, Binary) ->
    [I || <<I:Size/signed>> <= Binary];
dis_array_elements({uint,Size}, Binary) ->
    [I || <<I:Size/unsigned>> <= Binary];
dis_array_elements({float,Size}, Binary) ->
    [F || <<F:Size/float>> <= Binary].
    
%% collect all symbols (and merge)
%% currently the symbols are export entries
%% so return them as Addr => Name!
dis_symb([{<<"SYMB">>, Table} | Sections], Symb0) ->
    Symb1 = maps:from_list(
	      [{Value,Sym} || 
		  <<SymLen:8, Sym:SymLen/binary,
		    ValLen:8, Value:ValLen/signed-unit:8>> <= Table]),
    dis_symb(Sections, maps:merge(Symb0, Symb1));
dis_symb([_ | Sections], Symb) ->
    dis_symb(Sections, Symb);
dis_symb([], Symb) ->
    Symb.
    
