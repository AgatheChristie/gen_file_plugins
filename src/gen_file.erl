-module(gen_file).

-export([init/1, get_compile_time/0, string_to_term/1, get_modify_time/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = gen_file_prv:init(State),
    {ok, State1}.

-include_lib("kernel/include/file.hrl").

get_compile_time() ->
    case file:read_file("compile_time") of
        {ok, Bin} ->
            string_to_term(Bin);
        _ ->
            {{0,0,0},{0,0,0}}
    end.

string_to_term(String) ->
    S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [{return, list}, global]),
    case erl_scan:string(S ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        {error, _Err, _} -> undefined
    end.

get_modify_time(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            MTime;
        {error, enoent} ->
            0
    end.
