%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Pack chine into executable script
%%% @end
%%% Created : 15 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chine_pack).

-export([main/1, exec_type/1]).

%%
%% Usage:  chine_pack code.x
%%

main(_Args) ->
    Dir = code:priv_dir(chine),
    {ok,List} = file:list_dir(Dir),
    lists:foreach(
      fun(File="chine_exec."++_) ->
	      case emit_exec(filename:join(Dir,File)) of
		  {ok, Data} ->
		      io:put_chars(Data);
		  Error = {error,_} ->
		      io:format("unable to process ~s: ~p\n", [File,Error])
	      end;
	 (_) -> %% ignore other files
	      ok
      end, List).

emit_exec(File) ->
    case exec_type(File) of
	{ok, TypeMap} ->
	    {ok,Bin} = file:read_file(File),
	    %% fixme: compress
	    Data = make_rows(base64:encode(Bin), 76),
	    UName = make_uname(TypeMap),
	    {ok,
	     [["elif [ \"$SM\" = \"",UName,"\" ]; then\n"],
	      "(base64 -d | dd of=$0 conv=notrunc oflag=seek_bytes seek=0 2>/dev/null) <<50F645CD7C7209972B48C3220959677A\n",
	      Data,
	      "50F645CD7C7209972B48C3220959677A\n"
	      "exec $0\n"
	     ]};
	Error ->
	    Error
    end.

exec_type(File) ->
    case read_header(File, 64) of
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
	    [Line, "\n" | make_rows(Data1, LineLength)];
	<<>> ->
	    [];
	<<Line/binary>> ->
	    [Line, "\n"]
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
		  <<?MH_CIGAM:32/big,CPU:32,_/binary>> ->
		      {32,little,CPU};
		  <<?MH_MAGIC_64:32/big,CPU:32,_/binary>> ->
		      {64,big,CPU};
		  <<?MH_CIGAM_64:32/big,CPU:32,_/binary>> -> 
		      {64,little,CPU};
		  <<?FAT_MAGIC:32/big,CPU:32,_/binary>> ->
		      {fat,big,CPU};
		  <<?FAT_CIGAM:32/big,CPU:32,_/binary>> ->
		      {fat,little,CPU};
		  _ -> {0, unknown,0}
	      end,
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

%% Windows object code format
coff(Header) ->
    case Header of
	<<_:16#3c, "PE\0\0", Machine:16/little, _/binary>> ->
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
		    #{ operating_system => "Windows",
		       machine => M,
		       type => exe,
		       word_size => W,
		       endian => E }
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
			?EM_ARM -> "arm";
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
