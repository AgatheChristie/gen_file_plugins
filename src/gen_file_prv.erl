-module(gen_file_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_file).
-define(DEPS, [app_discovery]).
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 game"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "generate record fields"},
        {desc, "  生成 "}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% @doc 编写生成文件的模块时，只需要提供 start/0 接口，想重新编译需要删除gen_file/ebin下对应beam文件
    Dir = "_build/default/plugins/gen_file/src/generater",
    {ok, Files} = file:list_dir(Dir),
    [begin
         [Mod, "erl"] = string:split(File, "."),
         {T, _} = timer:tc(erlang, apply, [list_to_atom(Mod), start, [State]]),
         io:format("Mod: ~s UseTime: ~.2fms ~n", [Mod, T / 1000])
     end || File <- Files],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
