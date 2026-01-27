%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Compile CHI into CHINE
%%% @end
%%% Created : 20 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chi).

-export([file/1]).
-export([module/1]).
-export([code/2]).

-define(PROG, "chine").

-include("../include/chine.hrl").

file(Filename) ->
    case file:consult(Filename) of
	{ok,Code} ->	
	    module(Code);
	Error ->
	    Error
    end.

%% top level chi:
%% define
%% enum
%% import
%% comment
%% export
%% def
%% function

module(Code) ->
    Sym = #{ {symbol,'$DP'} => ?DP,
	     {symbol,'$TIB'} => ?TIB,
	     {symbol,'$IN'} => ?IN,
	     {symbol,'TIB_SIZE'} =>  ?TIB_SIZE},
    module_(Code,[],Sym).

module_([{define,Name,Value}|Code],Acc,Sym) ->
    module_(Code,Acc,maps:put({symbol,Name},Value,Sym));
module_([{enum,Ls}|Code],Acc,Sym) ->
    Sym1 = add_enums(Ls, Sym),
    module_(Code,Acc,Sym1);
module_([{comment,_Comment}|Code],Acc,Sym) ->
    module_(Code, Acc, Sym);
module_([{export,L}|Code],Acc,Sym) ->
    module_(Code, [{export,L}|Acc], Sym#{ {export,L} => true });
module_([{def, Name, Body}|Code], Acc, Sym) ->
    Sym1 = add_block(Sym, Name),
    {Stmts,_Sym2} = code(Body, Sym1),
    Def = [{label,Name}]++Stmts++[exit],
    module_(Code, [Def|Acc], Sym1);
module_([{function,Returns,Name,Args,Locals,Body}|Code], Acc, Sym) ->
    A = length(Args),
    true = A =< ?MAX_ARGS,
    R = length(Returns),
    true = R =< ?MAX_RETURNS,
    L = length(Locals),
    true = L =< ?MAX_LOCALS,
    AR = (A bsl 4) + R,
    AL = (A bsl 16) + L,

    Sym1 = add_block(Sym, Name),

    Sym2 = lists:foldl(
	     fun({Arg,I}, Sy) ->
		     Sy#{ {symbol,Arg} => I }
	     end, Sym1, lists:zip(Args, lists:seq(0, A-1))),
    Sym3 = lists:foldl(
	     fun({Var,I}, Sy) ->
		     Sy#{ {symbol,Var} => -I }
	     end, Sym2, lists:zip(Locals, lists:seq(1, L))),
    {Stmts,_Sym4} = code(Body, Sym3),
    Func = [{label,Name},{enter,AL}] ++ Stmts ++ [{leave,AR},exit],
    %% reverse Func?
    module_(Code, [Func|Acc], Sym1);
module_([{import, File}|Code],Acc,Sym) ->
    %% fixme: include once etc...
    case file:consult(File++".chi") of
	{ok,Fs0} ->
	    module_(Fs0 ++ Code, Acc, Sym);
	{error,Reason} ->
	    io:format(standard_error,
		      "~s:error: ~p\n", 
		      [?PROG, Reason]),
	    halt(1)
    end;
module_([Block|Code],Acc,Sym) when is_list(Block) -> %% chine code block
    {Acc1, Sym1} = code_(Block, Acc, Sym),
    module_(Code, Acc1, Sym1);
module_([],Acc,Sym) ->
    {lists:flatten(lists:reverse(Acc)), Sym}.

code(Code,Sym) ->
    {RevCode, Sym1} = code_(Code,[],Sym),
    {lists:reverse(RevCode), Sym1}.

code_([[]|Code],Acc,Sym) ->
    code_(Code,Acc,Sym);
code_([{comment,_Comment}|Code],Acc,Sym) ->
    code_(Code, Acc, Sym);
code_([{enum,Ls}|Code],Acc,Sym) ->
    %% local enums inside def/functions
    Sym1 = chine:add_enums(Ls, Sym),
    code_(Code, Acc, Sym1);
code_([Literal|Code],Acc,Sym) when is_integer(Literal) ->
    code_(Code,[Literal|Acc],Sym);
code_([{'_if',Then}|Code],Acc,Sym) ->
    {L,Sym1} = add_label(Sym),
    Block = [{jmpz,L},Then,{label,L}],
    code_(Block ++ Code,Acc,Sym1);
code_([{'_if',Then,Else}|Code],Acc,Sym) ->
    {L0,Sym1} = add_label(Sym),
    {L1,Sym2} = add_label(Sym1),
    Block = [{jmpz,L0},Then,{jmp,L1},
	     {label,L0},Else,{label,L1}],
     code_(Block++Code,Acc,Sym2);
code_([{'_again',Loop}|Code],Acc,Sym) ->
    {L0,Sym1} = add_label(Sym),
    Block = [{label,L0},Loop,{jmp,L0}],
    code_(Block++Code,Acc,Sym1);
code_([{'_until',Loop}|Code],Acc,Sym) ->
    {L0,Sym1} = add_label(Sym),
    Block = [{label,L0},Loop,{jmpz,L0}],
    code_(Block++Code,Acc,Sym1);
code_([{'_repeat',While,Loop}|Code],Acc,Sym) ->
    {L0,Sym1} = add_label(Sym),
    {L1,Sym2} = add_label(Sym1),
    Block = [{label,L0},While,{jmpz,L1},
	     Loop,{jmp,L0},{label,L1}],
    code_(Block++Code,Acc,Sym2);
code_([{'_for',Loop}|Code],Acc,Sym) ->
    {L0,Sym1} = add_label(Sym),
    Block = ['>r', {label,L0},Loop,{next,L0}],
    code_(Block++Code,Acc,Sym1);

code_([Ops|Code],Acc,Sym) when is_list(Ops) ->
    try unicode:characters_to_binary(Ops, utf8) of
	{error, _, _} ->
	    code_(Ops++Code,Acc,Sym);
	Utf8 when is_binary(Utf8) ->
	    code_(Code,[{string,Utf8}|Acc],Sym)
    catch
	error:_ ->
	    code_(Ops++Code,Acc,Sym)
    end;
code_([Op|Code],Acc,Sym) ->
    {Code1,Sym1} = stmnt_(Op,Sym),
    code_(Code, [Code1|Acc], Sym1);
code_(Op,Acc,Sym) when not is_list(Op) ->
    {Code1,Sym1} = stmnt_(Op,Sym),
    code_([], [Code1|Acc], Sym1);
code_([],Acc,Sym) ->
    {Acc,Sym}.

stmnt_({'if',Cond,Then},Sym) ->
    {L,Sym1} = add_label(Sym),
    code([Cond,{jmpz,L},Then,{label,L}],Sym1);
stmnt_({'if',Cond,Then,Else},Sym) ->
    {L0,Sym1} = add_label(Sym),
    {L1,Sym2} = add_label(Sym1),
    code([Cond,{jmpz,L0},
	  Then,{jmp,L1},
	  {label,L0},Else,
	  {label,L1}], Sym2);
stmnt_({'while',Cond,Body},Sym) ->
    {L0,Sym1} = add_label(Sym),
    {L1,Sym2} = add_label(Sym1),
    code([{label,L0},Cond,{jmpz,L1},
	  Body,
	  {jmp,L0},{label,L1}],Sym2);
stmnt_({'do',Body,Cond},Sym) ->
    {L0,Sym1} = add_label(Sym),
    code([{label,L0},
	  Body,
	  Cond,{jmpnz,L0}], Sym1);
stmnt_({const,Name},Sym) ->
    case lookup_const(Name, Sym) of
	{ok,Value} ->
	    {[Value],Sym};
	error ->
	    io:format(standard_error, "error: symbol ~p is not defined\n",
		      [Name]),
	    {[0],Sym}
    end;
stmnt_({call,Name,Args}, Sym) when is_list(Args) -> %% "high" level call
    code(lists:reverse(Args)++[{call,Name}], Sym);
stmnt_({op, ':=', Name, Expr}, Sym) when is_atom(Name) ->
    {Code1,Sym1} = code(Expr,Sym),
    case maps:find({symbol,Name}, Sym) of
	{ok,I} when is_integer(I) ->
	    {Code1++[{set,I}],Sym1};
	error ->
	    io:format(standard_error, "error: symbol ~p not defined\n",[Name]),
	    {[],Sym}
    end;
stmnt_({op, ':=', Ns, Expr}, Sym) when is_list(Ns), is_atom(hd(Ns)) ->
    {Code1,Sym1} = code(Expr,Sym),
    Assign = lists:foldl(
	       fun(Name, Acc) ->
		       case maps:find({symbol,Name}, Sym) of
			   {ok,I} ->
			       [{set,I}|Acc];
			   error ->
			       io:format(standard_error, 
					 "error: symbol ~p not defined\n",
					 [Name]),
			       Acc
		       end
	       end, [], Ns),
    {Code1++Assign, Sym1};

stmnt_({label,L},Sym) ->
    case maps:find({export,L}, Sym) of
	{ok,true} ->
	    {[{label,L}],Sym};
	error ->
	    Context = maps:get(context,Sym),
	    L1 = {Context,L},
	    {[{label,L1}],Sym#{ {label,L} => L1 }}
    end;
stmnt_({op,Op,Arg1,Arg2},Sym) ->
    code([Arg1,Arg2,Op], Sym);
stmnt_({op,Op,Arg1},Sym) ->
    code([Arg1,Op], Sym);
stmnt_(Op,Sym) ->
    case chine:opcode_type(Op, Sym) of
	call when is_atom(Op) ->
	    case maps:find({symbol,Op}, Sym) of
		{ok,I} ->
		    {[{get,I}],Sym};
		error ->
		    case maps:find(Op, synthetic_opcodes()) of
			{ok,Code} ->
			    %% expand more?
			    code(Code, Sym);
			error ->
			    {[Op],Sym}
		    end
	    end;
	none ->
	    io:format(standard_error,
		      "opcode ~p is not known\n", [Op]),
	    error({unknown_op, Op});
	jop ->
	    {Jmp, L} = Op,
	    case maps:find({label,L}, Sym) of
		{ok, L1} ->
		    {[{Jmp,L1}], Sym};
		error ->
		    case maps:find({export,L}, Sym) of		    
			{ok,true} ->
			    {[Op],Sym};
			error ->
			    Context = maps:get(context,Sym),
			    L1 = {Context,L},
			    {[{Jmp,L1}],Sym#{ {label,L} => L1 }}
		    end
	    end;
	_Type ->
	    {[Op],Sym}
    end.

%% add block level
add_block(Sym, Name) ->
    BlockNum = maps:get(block, Sym, 1),
    Sym#{ context => Name, block => BlockNum+1, label => 1}.

%% add label L<b>.<l>  <b> = block, <l> = local number
add_label(Sym) ->
    B = maps:get(block, Sym, 0),  %% globals = 0
    L = maps:get(label, Sym, 1),    %% local label 1...
    {"L"++integer_to_list(B)++"."++integer_to_list(L),
     Sym#{ label => L + 1}}.

lookup_const(Name, Sym) ->
    lookup_const_(Name, Sym, sets:new()).

lookup_const_(Name, Sym, Tried) ->
    case sets:is_element(Name, Tried) of
	true -> error;
	false ->
	    case maps:find({symbol,Name}, Sym) of
		error -> error;
		{ok,Int} when is_integer(Int) -> {ok,Int};
		{ok,Float} when is_float(Float) -> {ok,Float};
		{ok,Name1} -> 
		    lookup_const_(Name1, Sym, sets:add_element(Name, Tried))
	    end
    end.

%% enumerate symbols
add_enums(Es, Sym) ->
    add_enums_(Es, 0, Sym).

add_enums_([E|Es], I, Sym) ->
    Sym1 = maps:put({symbol,E}, I, Sym),
    add_enums_(Es, I+1, Sym1);
add_enums_([], _I, Sym) ->
    Sym.    

%% not real opcodes, they are expanded like macros
%% some may be expanded like calls instead?!
synthetic_opcodes() ->
    #{
      '1+'  => [1,'+'],
      '1-'  => [1,'-'],
      'lshift' => [shift],
      'rshift' => [negate,shift],
      '<'   => ['-', '0<'],
      '>'   => [swap, '-', '0<'],
      '<='  => ['-', '0<='],
      '>='  => [swap,'-', '0<='],
      '='   => ['-', '0='],
      '=='  => ['-', '0='],
      '!='  => ['-', '0=', 'not'],
      '<>'  => ['-', '0=', 'not'],
      'u<'  => ['2dup','xor','0<',
		{'_if',[swap,drop,'0<'],['-','0<']}],
      'u<=' => ['2dup','xor','0<',
		{'_if',[swap,drop,'0<'],['-','0<=']}],
      'u>'  => [swap, 'u<'],
      'u>=' => [swap, 'u<='],
      '0<>' => ['0=', 'not'],
      '0>'  => [0,'>'],
      '0<=' => [1,'-','0<'],
      'abs' => [dup,'0<',{'_if',[negate]}],
      'min' => [over,over,'<',{'_if',[drop],[swap,drop]}],
      'max' => [over,over,'<',{'_if',[swap,drop],[drop]}],
      'nip' => [swap,drop],
      'tuck' => [swap,over],
      '-rot' => [rot,rot],
      '2drop' => [drop,drop],
      '2dup'  => [over,over],
      '2*'    => [dup,'+'],
      'arshift'   => [dup,32,swap,'-',
		      -1,swap,shift,
		      '-rot', negate,shift, 'or'],
      '2/'    => [1,arshift],
      'sqr'   => [dup,'*'],
      'mod'   => ['2dup','/','*','-'],
      'jmp*'  => ['>r', exit],  %% ( caddr -- )
      ';'     => [exit],
      'alloc'  => ['sp@',swap,'-','sp!'],
      'here'   => [ ?DP, '@' ],
      'comma' => [here, swap, over, 'c!', '1+', ?DP, '!' ]
}.
