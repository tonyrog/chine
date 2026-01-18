%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Compile CHI into CHINE
%%% @end
%%% Created : 20 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chi).

-export([module/1, module/2]).
-export([code/2]).

module(Functions) ->
    module(Functions, #{}).

module([F={function,_Ret,_Name,_Args,_Locals,_Body}|Fs], Sym) ->
    {Code1, Sym1} = code(F, Sym),
    {Code2, Sym2} = module(Fs, Sym1),
    {Code1 ++ Code2, Sym2};
module([Decl | Fs], Sym) ->
    {Code1, Sym1} = code(Decl, Sym),
    {Code2, Sym2} = module(Fs, Sym1),
    {Code1 ++ Code2, Sym2};
module([], Sym) ->
    {[], Sym}.

code({function,Returns,Name,Args,Locals,Body}, Sym) ->
    A = length(Args),    %% max arguments = 0 ..255
    true = A < 16#100,
    R = length(Returns), %% max returns   = 0..15
    true = R < 16#10,
    L = length(Locals),
    true = L < 16#10000,  %% max locals = 0..65535
    AR = (A bsl 4) + R,
    Sym1 = lists:foldl(
	     fun({Arg,I}, Sy) ->
		     Sy#{ {symbol,Arg} => I }
	     end, Sym, lists:zip(Args, lists:seq(0, A-1))),
    Sym2 = lists:foldl(
	     fun({Var,I}, Sy) ->
		     Sy#{ {symbol,Var} => -I }
	     end, Sym1, lists:zip(Locals, lists:seq(1, L))),
    {Stmts,Sym3} = code(Body, Sym2),
    Code = [{label,Name},{fenter,L}] ++ Stmts ++ [{fleave,AR},exit],
    %%{Code1,_} = chine:expand_synthetic(Code, chine_opt:create(chine:options())),
    {Code, Sym3};
code(Code,Sym) ->
    code_(Code,[],Sym).

code_([[]|Code],Acc,Sym) ->
    code_(Code,Acc,Sym);
code_([Ops|Code],Acc,Sym) when is_list(Ops) ->
    try erlang:iolist_to_binary(Ops) of
	Bin ->
	    code_(Code,[{string,binary_to_list(Bin)}|Acc],Sym)
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
    {lists:flatten(lists:reverse(Acc)),Sym}.

stmnt_({'if',Cond,Then},Sym) ->
    L = chine:new_label(),
    code([Cond,{{jop,jmpz},L},Then,{label,L}],Sym);
stmnt_({'if',Cond,Then,Else},Sym) ->
    L0 = chine:new_label(),
    L1 = chine:new_label(),
    code([Cond,{{jop,jmpz},L0},
	     Then,{{jop,jmp},L1},
	     {label,L0},Else,
	     {label,L1}], Sym);
stmnt_({'while',Cond,Body},Sym) ->
    L0 = chine:new_label(),
    L1 = chine:new_label(),
    code([{label,L0},Cond,{{jop,jmpz},L1},
	    Body,
	    {{jop,jmp},L0},{label,L1}],Sym);
stmnt_({'do',Body,Cond},Sym) ->
    L0 = chine:new_label(),
    code([{label,L0},
	  Body,
	  Cond,{{jop,jmpnz},L0}], Sym);
stmnt_({enum,Ls},Sym) ->
    Sym1 = chine:add_enums(Ls, Sym),
    {[],Sym1};
stmnt_({define,Name,Value},Sym) ->
    {[],maps:put({symbol,Name},Value,Sym)};
stmnt_({comment,_Comment},Sym) ->
    %% just a comment ignore it
    {[],Sym};
stmnt_({Jop,L},Sym) when
      Jop =:= jmpz; Jop =:= jmpnz;
      Jop =:= next; Jop =:= jmplz;
      Jop =:= jmp; Jop =:= call ->
    L1 = chine:normalize_label(L),
    {[{{jop,Jop},L1}], Sym};
stmnt_(Op={const,C},Sym) when is_integer(C) ->
    {[Op], Sym};
stmnt_({const,Name},Sym) ->
    case maps:find({symbol,Name}, Sym) of
	{ok,C} ->
	    {[{const,C}],Sym};
	error ->
	    io:format(standard_error, "error: symbol ~p not defined\n",[Name]),
	    {[{const,Name}],Sym}
    end;
stmnt_({arg,I},Sym) when is_integer(I) ->
    {[{arg,I}],Sym};
stmnt_({arg,Name},Sym) ->
    case maps:find({symbol,Name}, Sym) of
	{ok,I} ->
	    {[{arg,I}],Sym};
	error ->
	    io:format(standard_error, "error: symbol ~p not defined\n",[Name]),
	    {[{arg,Name}],Sym}
    end;
stmnt_({call,Name,Args}, Sym) when is_list(Args) ->
    code(lists:reverse(Args)++[{call,Name}], Sym);
stmnt_({op, ':=', Name, Expr}, Sym) ->
    {Code1,Sym1} = code(Expr,Sym),
    case maps:find({symbol,Name}, Sym) of
	{ok,I} ->
	    {Code1++['fp@',{const,I},'+','!'],Sym1};
	error ->
	    io:format(standard_error, "error: symbol ~p not defined\n",[Name]),
	    {[],Sym}
    end;
stmnt_({export,L},Sym) ->
    L1 = chine:normalize_label(L),
    Sym1 = maps:put({export,L1}, 0, Sym),
    {[{export,L1}],Sym1};
stmnt_({label,L},Sym) ->
    L1 = chine:normalize_label(L),
    {[{label,L1}],Sym};
stmnt_({op,Op,Arg1,Arg2},Sym) ->
    {Code1,Sym1} = code(Arg1,Sym),
    {Code2,Sym2} = code(Arg2,Sym1),
    {Code1++Code2++[Op], Sym2};
stmnt_({op,Op,Arg1},Sym) ->
    {Code1,Sym1} = code(Arg1,Sym),
    {Code1++[Op],Sym1};
stmnt_(Op,Sym) when is_tuple(Op) ->
    {[Op],Sym};
stmnt_(Op,Sym) when is_atom(Op) ->
    Map = chine:synthetic_opcodes(),
    case maps:find(Op,Map) of
	error -> %% check if it is a argument name
	    case maps:find({symbol,Op}, Sym) of
		{ok,I} ->
		    {{arg,I},Sym};
		error ->
		    {[Op],Sym}
	    end;
	{ok,Ops} when is_list(Ops) ->
	    code(Ops, Sym)
    end;
stmnt_(Int,Sym) when is_integer(Int) ->
    {[{const,Int}],Sym}.
