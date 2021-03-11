%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Disassembler
%%% @end
%%% Created : 11 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chine_disasm).

-export([file/1]).

-include("../include/chine.hrl").

file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    dis(Bin);
	Error ->
	    Error
    end.

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
    {Addr1,Code1} = dis_opcodes(Content, Addr, SymTab, Code0),
    dis_code(Sections, Addr1, SymTab, Code1);
dis_code([_|Sections], Addr1, SymTab, Code1) ->
    dis_code(Sections, Addr1, SymTab, Code1);
dis_code([], Addr1, SymTab, Code) ->
    {Addr1,lists:reverse(Code),SymTab}.

-define(ARRAY, 7).
-define(SYS, 25).


dis_opcodes(<<>>, Addr, _SymTab, Code) ->
    {Addr, Code};
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
    dis_opcodes(Content, Addr+2, SymTab, [OpA|Code]);    
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
	      _ -> {{unknown,OP}}
	  end,
    dis_opcodes(Content, Addr+1, SymTab, [OpA|Code]);
dis_opcodes(<<1:2, L:2, ?ARRAY:4, Len:(1 bsl L), String:Len/binary,
	      Content/binary>>,
	    Addr, SymTab, Code) ->
    %% uint3x8  0-7 8 bit characters
    Op = {array,{uint3x8,Len},String},
    dis_opcodes(Content, Addr+1+Len, SymTab, [Op|Code]);
dis_opcodes(<<1:2, L:2, JOP:4, A:(1 bsl L)/signed-unit:8, Content/binary>>,
	    Addr, SymTab, Code) ->

    {Int,UInt} = case L of
		     0 -> {int8,uint8};
		     1 -> {int16,uint16};
		     2 -> {int32,uint32};
		     3 -> {int64,uint64}
		 end,
    OpA = case JOP+2 of
	      #jopcode.jmpz -> {jmpz,{Int,A}};
	      #jopcode.jmpnz -> {jmpnz,{Int,A}};
	      #jopcode.next -> {next,{Int,A}};
	      #jopcode.jmplz -> {jmplz,{Int,A}};
	      #jopcode.jmp   -> {jmp,{Int,A}};
	      #jopcode.call  -> {call,{Int,A}};
	      #jopcode.literal -> {literal,{Int,A}};
	      #jopcode.array   -> {array,{UInt,unsigned(UInt,A)}};
	      #jopcode.arg     -> {arg,{Int,A}};
	      _ -> {{unknown,JOP},{Int,A}}
	  end,
    dis_opcodes(Content, Addr+1+(1 bsl L), SymTab, [OpA|Code]);
dis_opcodes(<<2:2, V:3, ?ARRAY:3, Content/binary>>, Addr, SymTab, Code) ->
    {L,E,Variant} =
	case V of
	    0 -> {1, 1, uint8x8};
	    1 -> {1, 2, uint8x16};
	    2 -> {1, 4, uint8x32};
	    4 -> {2, 2, uint16x8};
	    5 -> {2, 2, uint16x16};
	    6 -> {2, 4, uint16x32}
	end,
    EL = (1 bsl E),
    LL = (1 bsl L),
    <<Len:LL, Data:(Len*L)/binary, Content1/binary>> = Content,
    Array = [Ei || <<Ei:EL>> <= Data],
    Op = {array,{Variant,Len},Array},
    dis_opcodes(Content1, Addr+1+L+Len*LL, SymTab, [Op|Code]);
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
	      #jopcode.array   -> {array,{uint3,unsigned(uint3,A)}}
	  end,
    dis_opcodes(Content, Addr+1, SymTab, [OpA|Code]);

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
    dis_opcodes(Content, Addr+1, SymTab, [OpB,OpA|Code]).

unsigned(uint3, V) when V < 0 -> V+(1 bsl 3);
unsigned(uint8, V) when V < 0 -> V+(1 bsl 8);
unsigned(uint16, V) when V < 0 -> V+(1 bsl 16);
unsigned(uint32, V) when V < 0 -> V+(1 bsl 32);
unsigned(_, V) when V >= 0 -> V.

%% collect all symbols (and merge)
dis_symb([{<<"SYMB">>, Table} | Sections], Symb0) ->
    Symb1 = maps:from_list(
	      [{Sym,Value} || 
		  <<SymLen:8, Sym:SymLen/binary,
		    ValLen:8, Value:ValLen/signed-unit:8>> <= Table]),
    dis_symb(Sections, maps:merge(Symb0, Symb1));
dis_symb([_ | Sections], Symb) ->
    dis_symb(Sections, Symb);
dis_symb([], Symb) ->
    Symb.


    
    
