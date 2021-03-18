%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    getopt 
%%% @end
%%% Created : 18 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(chine_opt).

-export([parse/2]).
-export([value/2, value/3]).
-export([usage/2, short_usage/2]).
-export([format_usage/2]).
-export([format_short_usage/1]).
-export([format_error/3]).

%% create default from option list
-export([create/1]).

value(Key, Opts) ->
    maps:get(Key, Opts).

value(Key, Opts, Default) ->
    maps:get(Key, Opts, Default).

parse(SpecList, Args) ->
    parse_options(Args, SpecList, [], create(SpecList)).

parse_options([[$-,$-|String]|Args], SpecList, Acc, Opts) ->
    parse_long_option(String, Args, SpecList, Acc, Opts);
parse_options([[$-|String]|Args], SpecList, Acc, Opts) ->
    parse_short_option(String, Args, SpecList, Acc, Opts);
parse_options([Arg|Args], SpecList, Acc, Opts) ->
    parse_options(Args, SpecList, [Arg|Acc], Opts);
parse_options([], _SpecList, Acc, Opts) ->
    {ok, {Opts, lists:reverse(Acc)}}.

%% long option
%%  --key=value
%%  --key value
parse_long_option(String, Args, SpecList, Acc, Opts) ->    
    case string:chr(String, $=) of
	0 ->
	    parse_long_option_(String, Args, SpecList, Acc, Opts);
	I -> 
	    Name=string:substr(String,1,I-1),
	    Value=string:substr(String,I+1),
	    parse_long_option_(Name, [Value|Args], SpecList, Acc, Opts)
    end.

parse_long_option_(Long, Args, SpecList, Acc, Opts) ->
    case find_long(Long, SpecList) of
	false ->
	    {error, ["unknown option --",Long]};
	#{ key := Key, type := Type } ->
	    if Args =:= [], type =:= boolean ->
		    parse_options(Args, SpecList, Acc, Opts#{ Key => true });
	       Args =:= [] ->
		    {error, ["value range ", [Long]]};
	       true ->
		    Args1 = tl(Args),
		    case parse_option_value(Type, hd(Args)) of
			false when Type =:= boolean ->
			    parse_options(Args1, SpecList, Acc,
					  Opts#{ Key => true });
			false ->
			    {error, ["value range ", [Long]]};
			{true,Value} ->
			    parse_options(Args1, SpecList, Acc, 
					  Opts#{ Key => Value })
		    end
	    end
    end.

%% short option
%%   -k value
%%   -kvalue
%%   -k[lmn]        (boolean)
parse_short_option([Short|StrValue], Args, SpecList, Acc, Opts) ->
    case find_short([Short], SpecList) of
	false ->
	    {error, ["unknown option ",[$-,Short]]};
	Spec when StrValue =:= "" ->
	    parse_short_value(Spec, Args, SpecList, Acc, Opts);
	#{ key := Key, type := Type } ->
	    case parse_option_value(Type, StrValue) of
		false when Type =:= boolean ->
		    parse_short_option(StrValue,Args,SpecList,Acc,
				       Opts#{ Key => true });
		false ->
		    {error, ["value range ", [Short]]};
		{true, Value} ->
		    parse_options(Args, SpecList, Acc, Opts#{ Key => Value })
	    end
    end.

parse_short_value(#{ key := Key, short := Short, type := Type}, 
		  Args0=[StrValue|Args], SpecList, Acc, Opts) ->
    case parse_option_value(Type, StrValue) of
	false when Type =:= boolean ->
	    parse_options(Args0, SpecList, Acc, Opts#{ Key => true });
	false ->
	    {error, ["value range ", [Short]]};
	{true, Value} ->
	    parse_options(Args, SpecList, Acc, Opts#{ Key => Value })
    end;
parse_short_value(#{ key := Key, short := Short, type := Type},
		  [], SpecList, Acc, Opts) ->
    if Type =:= boolean ->
	    parse_options([], SpecList, Acc, Opts#{ Key => true });
       true ->
	    {error, ["value range ", [Short]]}
    end.

find_long(Name, [Spec=#{ long := Name} | _]) ->
    Spec;
find_long(Name, [_|SpecList]) ->
    find_long(Name, SpecList);
find_long(_Name, []) ->
    false.

find_short(Name, [Spec=#{ short := Name} | _]) ->
    Spec;
find_short(Name, [_|SpecList]) ->
    find_short(Name, SpecList);
find_short(_Name, []) ->
    false.

parse_option_value(boolean, "1") -> {true,true};
parse_option_value(boolean, "0") -> {true,false};
parse_option_value(integer, Value) ->
    case string:to_integer(Value) of
	{Int,[]} -> {true, Int};
	_ -> false
    end;
parse_option_value(float, Value) ->
    case string:to_integer(Value) of
	{Float,[]} -> {true,Float};
	_ -> false
    end;
parse_option_value(_Type, [$-|_]) -> false;
parse_option_value(string, Value) -> {true,Value};
parse_option_value(atom, Value) -> {true,list_to_atom(Value)};
parse_option_value(_, _) -> false.

%% Setup default values
create(SpecList) ->
    create(SpecList, #{}).
create([Spec = #{ key := Key} |SpecList], Opts) ->
    create(SpecList, 
	   Opts#{ Key => maps:get(default, Spec, undefined)});
create([], Opts) ->
    Opts.

format_error(_SpecList,Prog,Error) ->
    [Prog,":","error ", Error].

usage(SpecList, Prog) ->
    io:put_chars(format_usage(SpecList, Prog)).

short_usage(SpecList, Prog) ->
    io:put_chars(["Usage: ",Prog," ", format_short_usage(SpecList), "\n"]).

format_usage(SpecList, Prog) ->
    ["Usage: ", Prog, " ", format_short_usage(SpecList), "\n",
     [begin
	  Short = maps:get(short, Spec),
	  Long  = maps:get(long, Spec),
	  Description = maps:get(description, Spec, ""),
	  ["  -",Short,", --",Long, fill(length(Long),15), " ",
	   Description,"\n"]
      end || Spec <- SpecList]].

%% FIXME? collect boolean in one group?
format_short_usage([Spec|SpecList]) ->
    Short = maps:get(short, Spec),
    Key   = maps:get(key, Spec),
    case maps:get(type, Spec) of
	boolean ->
	    [["[",[$-|Short],"] "] | 
	     format_short_usage(SpecList)];
	_ ->
	    [["[",[$-|Short]," <",atom_to_list(Key),">] "] |
	     format_short_usage(SpecList)]
    end;
format_short_usage([]) ->
    [].

fill(N, W) when N < W ->
    lists:duplicate(W-N, $\s);
fill(_, _) ->
    "".



