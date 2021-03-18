%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Pack chine into executable script
%%% @end
%%% Created : 15 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chine_pack).

-export([start/1, exe_type/1]).
-export([pack/2]).

-define(NL, "\n").
%% -define(NL, "\r\n").

-define(HERE, "50F645CD7C7209972B48C3220959677A").
-define(LINE_LENGTH, 76).

%%
%% Usage:  chine_pack code.x code
%%

start([ChineFile]) ->
    pack(ChineFile, standard_io),
    halt(0);
start([ChineFile,OutFile]) ->
    pack(ChineFile,OutFile),
    halt(0);
start(_) ->
    io:format("usage: chine_pack file.x [command.sh]\n").

pack(ChineFile,standard_io) ->
    pack_(ChineFile,standard_io);
pack(ChineFile,OutFile) ->
    case file:open(OutFile, [write]) of
	{ok,Fd} ->
	    try pack_(ChineFile,Fd) of
		Res -> Res
	    after
		file:close(Fd)
	    end;
	{error,Reason} ->
	    io:format("file error: unable to open output file ~p : ~p\n",
		      [OutFile, Reason]),
	    halt(1)
    end.
	    
pack_(ChineFile,Fd) ->
    Dir = code:priv_dir(chine),
    {ok,DirList} = file:list_dir(Dir),
    ExeList = 
	lists:foldl(
	  fun(File="chine_exec."++_, Acc) ->
		  ExeFile = filename:join(Dir, File),
		  %% io:format("Added ~s\n", [ExeFile]),
		  case read_exe(ExeFile) of
		      {ok,Exe} ->
			  [Exe|Acc];
		      Error = {error,_} ->
			  io:format("unable to read ~s: ~p\n", [ExeFile,Error])
		  end;
	     (_, Acc) -> Acc %% ignore other files
	  end, [], DirList),
    ZeroSize = lists:max([byte_size(Bin) || {_TypeMap,Bin} <- ExeList]),
    LineBytes = ?LINE_LENGTH + length(?NL),
    NZBytes = ((ZeroSize + LineBytes - 1) div LineBytes)*?LINE_LENGTH,
    Zs = erlang:iolist_to_binary(lists:duplicate(NZBytes,$0)),
    ZData = make_rows(Zs, ?LINE_LENGTH),
    %% io:format("ZeroSize = ~w\n", [ZeroSize]),
    %% io:format("iolist_size(ZData) = ~w\n", [iolist_size(ZData)]),
    io:put_chars(Fd,
		 ["#!/bin/bash", ?NL,
		  "SM=`uname -s`-`uname -m`",?NL,
		  "chmod -f +wx $0",?NL,
		  "if [ -n \"\" ]; then", ?NL,
		  "true <<", ?HERE, ?NL,
		  ZData,
		  ?HERE, ?NL]),
    %% Output executables
    lists:foreach(
      fun({TypeMap,Bin}) ->
	      Data = format_exe(TypeMap,Bin),
	      io:put_chars(Fd, Data)
      end, ExeList),
    %% Output chine code
    {ok,Chine} = file:read_file(ChineFile),
    Chine1 = zeropad(Chine, 38),
    ChineData = format_hex(Chine1),  %% store chine code as hex data
    %% io:format("Chine code size = ~w padded to ~w\n", 
    %%   [byte_size(Chine), byte_size(Chine1)]),
    %% 8 hex characters for as offset to program start
    Tail = erlang:iolist_to_binary(
	     [ChineData,
	      ?HERE, ?NL,
	      "fi", ?NL,
	      ": "]),
    TailLen = tl(integer_to_list(16#100000000+byte_size(Tail)+9,16)),
    io:put_chars(Fd,
		 ["else", ?NL,
		  "true <<", ?HERE, ?NL,
		  Tail, TailLen, ?NL]).

zeropad(Bin, M) ->
    Size = byte_size(Bin),
    Pad  = (M - (Size rem M)) rem M,
    <<Bin/binary, 0:Pad/unit:8>>.

read_exe(File) ->
    case exe_type(File) of
	{ok, TypeMap} ->    
	    {ok,Bin} = file:read_file(File),
	    {ok,{TypeMap,Bin}};
	Error ->
	    Error
    end.

format_exe(TypeMap, Bin) ->
    Data = format_gzip_base64(Bin),
    UName = make_uname(TypeMap),
    %% oflag=seek_bytes (not needed, not avail on Darwin 17.7)
    DD = "dd of=$0 conv=notrunc seek=0 2>/dev/null",
    Base64 = "base64 --decode",
    UnZip = "gunzip",
    [["elif [ \"$SM\" = \"",UName,"\" ]; then", ?NL],
     "( ", Base64, " | ", UnZip, " | ", DD, ") <<", ?HERE, ?NL,
     Data,
     ?HERE, ?NL,
     "exec $0 $0", ?NL
    ].

format_hex(Bin) ->
    make_rows(hex_encode(Bin), ?LINE_LENGTH).

format_gzip_base64(Bin) ->
    Bin1 = zlib:gzip(Bin),
    make_rows(base64:encode(Bin1), ?LINE_LENGTH).

hex_encode(Binary) ->
    erlang:iolist_to_binary(hex_encode_(Binary)).

hex_encode_(<<H:4,L:4,Bin/binary>>) ->
    Hex = {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$A,$B,$C,$D,$E,$F},
    [element(H+1,Hex),element(L+1,Hex)|hex_encode_(Bin)];
hex_encode_(<<>>) ->
    [].

exe_type(File) ->
    case read_header(File, 256) of
	{ok, Header} ->
	    case elf(Header) of
		{true, TypeMap} ->
		    {ok, TypeMap};
		false ->
		    case macho(Header) of
			{true, TypeMap} ->
			    {ok, TypeMap};
			false ->
			    case coff(Header) of
				{true, TypeMap} ->
				    {ok, TypeMap};
				false ->
				    {error, unknown_type}
			    end
		    end
	    end;
	Error ->
	    Error
    end.

make_rows(Data, LineLength) ->
    case Data of
	<<Line:LineLength/binary, Data1/binary>> ->
	    [Line, ?NL | make_rows(Data1, LineLength)];
	<<>> ->
	    [];
	<<Line/binary>> ->
	    [Line, ?NL]
    end.

make_uname(#{ operating_system := S, machine := M }) ->
    S ++ "-" ++ M.

-define(MH_MAGIC,    16#feedface). %% the mach magic number 
-define(MH_CIGAM,    16#cefaedfe). %% NXSwapInt(MH_MAGIC) 
-define(MH_MAGIC_64, 16#feedfacf). %% the 64-bit mach magic number 
-define(MH_CIGAM_64, 16#cffaedfe). %% NXSwapInt(MH_MAGIC_64) 
-define(FAT_MAGIC,   16#cafebabe).
-define(FAT_CIGAM,   16#bebafeca).

-define(CPU_ARCH_MASK,	16#ff000000).		%% mask for architecture bits 
-define(CPU_ARCH_ABI64,	16#01000000).		%% 64 bit ABI 

-define(CPU_TYPE_X86,		(7)).
-define(CPU_TYPE_I386,		?CPU_TYPE_X86).		%% compatibility 
-define(CPU_TYPE_X86_64,	(?CPU_TYPE_X86 bor ?CPU_ARCH_ABI64)).
-define(CPU_TYPE_ARM,	        (12)).
-define(CPU_TYPE_POWERPC,	(18)).
-define(CPU_TYPE_POWERPC64,	(?CPU_TYPE_POWERPC bor ?CPU_ARCH_ABI64)).

macho(Header) ->
    {W,E,C} = case Header of
		  <<?MH_MAGIC:32/big, CPU:32, _/binary>> ->
		      {32,big,CPU};
		  <<?MH_CIGAM:32/big, CPU:32/little,_/binary>> ->
		      {32,little,CPU};
		  <<?MH_MAGIC_64:32/big, CPU:32,_/binary>> ->
		      {64,big,CPU};
		  <<?MH_CIGAM_64:32/big,CPU:32/little,_/binary>> -> 
		      {64,little,CPU};
		  <<?FAT_MAGIC:32/big,CPU:32,_/binary>> ->
		      {fat,big,CPU};
		  <<?FAT_CIGAM:32/big,CPU:32/little,_/binary>> ->
		      {fat,little,CPU};
		  _ -> {0, unknown,0}
	      end,
    %% io:format("macho w=~w, e=~w, c=~w\n", [W,E,C]),
    M = case C of
	    ?CPU_TYPE_I386   -> "i386";
	    ?CPU_TYPE_X86_64  -> "x86_64";
	    ?CPU_TYPE_ARM -> "arm";
	    ?CPU_TYPE_POWERPC -> "powerpc";
	    ?CPU_TYPE_POWERPC64 -> "powerpc64";
	    _ -> ""
	end,
    if M =:= "" ->
	    false;
       true ->
	    {true,
	     #{ operating_system => "Darwin",
		machine => M,
		type => exe,
		word_size => W,
		endian => E }}
    end.

-define(SHORT(X),  X:16/signed-little).
-define(USHORT(X), X:16/unsigned-little).
-define(DWORD(X),  X:32/unsigned-little).
-define(ULONG(X),  X:32/unsigned-little).
-define(LONG(X),   X:32/signed-little).

-define(USHORT_(N), _:N/unit:16).
-define(ULONG_(N), _:N/unit:32).

%% e_magic
-define(IMAGE_DOS_SIGNATURE,        16#5A4D).      %% MZ
-define(IMAGE_OS2_SIGNATURE,        16#454E).      %% NE
-define(IMAGE_OS2_SIGNATURE_LE,     16#454C).      %% LE
-define(IMAGE_NT_SIGNATURE,         16#00004550).  %% PE00

-define(SIZE_OF_NT_SIGNATURE, 4). %% sizeof (DWORD) - 32 bit

-define(IMAGE_DOS_HEADER,
	?USHORT(E_magic),         %% Magic number
	?USHORT(E_cblp),          %% Bytes on last page of file
	?USHORT(E_cp),            %% Pages in file
	?USHORT(E_crlc),          %% Relocations
	?USHORT(E_cparhdr),       %% Size of header in paragraphs
	?USHORT(E_minalloc),      %% Minimum extra paragraphs needed
	?USHORT(E_maxalloc),      %% Maximum extra paragraphs needed
	?USHORT(E_ss),            %% Initial (relative) SS value
	?USHORT(E_sp),            %% Initial SP value
	?USHORT(E_csum),          %% Checksum
	?USHORT(E_ip),            %% Initial IP value
	?USHORT(E_cs),            %% Initial (relative) CS value
	?USHORT(E_lfarlc),        %% File address of relocation table
	?USHORT(E_ovno),          %% Overlay number
	?USHORT_(4),              %% E_res[4] Reserved words
	?USHORT(E_oemid),         %% OEM identifier (for e_oeminfo)
	?USHORT(E_oeminfo),       %% OEM information; e_oemid specific
	?USHORT_(10),             %% e_res2[10] Reserved words
	?LONG(E_lfanew)           %% File address of new exe header
	).

-define(IMAGE_FILE_HEADER, 
	?USHORT(Machine),
	?USHORT(NumberOfSections),
	?ULONG(TimeDateStamp),
	?ULONG(PointerToSymbolTable),
	?ULONG(NumberOfSymbols),
	?USHORT(SizeOfOptionalHeader),
	?USHORT(Characteristics)
	).

%% Windows object code format
coff(Header) ->
    case Header of
	<<?IMAGE_DOS_HEADER,_/binary>> when E_magic =:= ?IMAGE_DOS_SIGNATURE ->
	    %% io:format("e_lfanew: ~w\n", [E_lfanew]),
	    case Header of
		<<_:E_lfanew/binary,
		  ?USHORT(?IMAGE_OS2_SIGNATURE),_/binary>> ->
		    {true, 
		     #{ operating_system => "OS/2",
			machine => "x86",
			type => exe,
			word_size => 16,
			endian => big }};
		<<_:E_lfanew/binary,
		  ?USHORT(?IMAGE_OS2_SIGNATURE_LE),_/binary>> ->
		    {true,
		     #{ operating_system => "OS/2",
			machine => "x86",
			type => exe,
			word_size => 16,
			endian => little }};
		<<_:E_lfanew/binary, ?DWORD(?IMAGE_NT_SIGNATURE),
		  ?IMAGE_FILE_HEADER, _/binary>> ->
		    {M,W,E} =
			case Machine of
			    16#14c  -> {"i386",       32, little};
			    16#8664 -> {"x86_64",     64, little};
			    16#1c0  -> {"arm",        32, little};
			    16#aa64 -> {"arm64",      64, little};
			    16#1c2  -> {"thumb",      16,little};
			    16#1c4  -> {"thumb2",     16,little};
			    16#5032 -> {"riscv32",    32, little};
			    16#5064 -> {"riscv64",    64, little};
			    16#50128-> {"riscv128",   128, little};
			    _ -> {"", 0, unknown}
			end,
		    if M =:= "" ->
			    false;
		       true ->
			    {true,
			     #{ operating_system => "Windows",
				machine => M,
				type => exe,
				word_size => W,
				endian => E }}
		    end;
		_ ->
		    {true,
		     #{ operating_system => "Dos",
		       machine => "x86",
		       type => exe,
		       word_size => 16,
		       endian => little }}
	    end;
	_ ->
	    false
    end.

-define(EV_CURRENT,	1).		%% Current version 

-define(ET_EXEC,	2).		%% Executable file 
-define(ET_DYN,		3).		%% Shared object file 

-define(EI_VERSION,	6).

-define(ELFDATANONE,    0).	%% Invalid data encoding 
-define(ELFDATA2LSB,    1).	%% 2's complement, little endian 
-define(ELFDATA2MSB,    2).	%% 2's complement, big endian 

-define(ELFCLASSNONE,   0).	%% Invalid class 
-define(ELFCLASS32,     1).	%% 32-bit objects 
-define(ELFCLASS64,     2).	%% 64-bit objects 

-define(EM_386,		 3).		%% Intel 80386 
-define(EM_X86_64,	62).		%% AMD x86-64 architecture 
-define(EM_ARM,		40).		%% ARM
-define(EM_RISCV,      243).            %% RISC-V

elf(Header) ->
    case Header of
	<<"\177ELF",
	  EI_CLASS, EI_DATA, ?EV_CURRENT, _EI_OSABI,
	  _EI_ABIVERSION, _EI_PAD, _, _, _, _, _, _,
	  TypeMachine:4/binary, _/binary>> ->
	    Endian = if EI_DATA == ?ELFDATA2LSB -> little;
			EI_DATA == ?ELFDATA2MSB -> big;
			EI_DATA == ?ELFDATANONE -> none
		     end,
	    WSize = if EI_CLASS == ?ELFCLASS32 -> 32;
		       EI_CLASS == ?ELFCLASS64 -> 64;
		       EI_CLASS == ?ELFCLASSNONE -> 0
		    end,
	    if Endian =:= little ->
		    <<Type:16/little,Machine:16/little>> = TypeMachine;
	       Endian =:= big ->
		    <<Type:16/big,Machine:16/big>> = TypeMachine
	    end,
	    {true,
	     #{ operating_system => "Linux",
		machine =>
		    case Machine of
			?EM_386    -> "i386";
			?EM_X86_64 -> "x86_64";
			?EM_ARM -> "armv7l";  %% fixme subtype!
			?EM_RISCV -> "riscv";
			_ -> Machine
		    end,
		type => 
		    case Type of
			?ET_EXEC -> exe;
			?ET_DYN -> dyn;
			_ -> Type
		    end,
		word_size => WSize,
		endian => Endian 
	      }};
	_ ->
	    false
    end.

read_header(File, N) ->
    case file:open(File, [binary]) of
	{ok,Fd} ->
	    Res = file:read(Fd, N),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.
