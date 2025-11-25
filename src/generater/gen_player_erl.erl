%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 18. 10月 2023 16:33
%%%-------------------------------------------------------------------
-module(gen_player_erl).
-author("Jeson").

-export([start/1, gen_player_erl/0]).

-define(PLAYER_HRL, "apps/game_server/include/player.hrl").

start(_State) -> ok.
  %%  MTime = gen_file:get_modify_time(?PLAYER_HRL),
  %%  CompileTime = gen_file:get_compile_time(),
  %%  (MTime == 0 orelse MTime > CompileTime) andalso gen_player_erl().

gen_player_erl() ->
    Fields = parse_player_fields(),
    FunBin = gen_player_function(Fields, <<>>),
    CacheBin = gen_cache_function(Fields, <<>>),
    RecordBin = gen_record_function(Fields, <<>>),
    FieldsBin = gen_fields_function(Fields, <<>>),
    FieldsBin1 = <<"all_fields() ->\n    [", FieldsBin/binary, "].\n\n">>,
    FileBin = <<(header())/binary, FunBin/binary, "\n",
        CacheBin/binary, RecordBin/binary, FieldsBin1/binary, (tail())/binary>>,
    ok = file:write_file("apps/game_server/src/player/player.erl", FileBin).

parse_player_fields() ->
    {ok, Bin} = file:read_file("apps/game_server/include/player.hrl"),
    {match, [_, FieldsBin]} = re:run(Bin, <<".*doc player_data.*?\n(.*?)}\\).*">>, [dotall, {capture, all, binary}]),
    FieldsBin1 = re:replace(FieldsBin, <<"\s|%.*?\n|\n">>, <<"">>, [global, {return, binary}]),
    [begin
         case binary:split(XField, <<"=">>) of
             [Field] ->
                 {Field, undefined};
             [Field, Default] ->
                 {Field, Default}

         end
     end || XField <- binary:split(FieldsBin1, <<",">>, [global, trim_all])].

gen_player_function([], Bin) ->
    Bin;
gen_player_function([{Field, undefined} | Rest], Bin) ->
    GetFunBin = gen_get_function(Field),
    SetFunBin = gen_set_function(Field),
    Bin1 = <<Bin/binary, GetFunBin/binary, SetFunBin/binary, "\n">>,
    gen_player_function(Rest, Bin1);
gen_player_function([{Field, _} | Rest], Bin) ->
    GetFunBin = gen_get_function_with_key(Field),
    SetFunBin = gen_set_function_with_key(Field),
    Bin1 = <<Bin/binary, GetFunBin/binary, SetFunBin/binary, "\n">>,
    gen_player_function(Rest, Bin1).

gen_get_function(Field) ->
    <<"
get_", Field/binary, "(Player) -> get(Player, #player.", Field/binary, ").">>.

gen_get_function_with_key(Field) ->
    <<"
get_", Field/binary, "(Player, Key) -> get(Player, #player.", Field/binary, ", Key).">>.

gen_set_function(Field) ->
   <<"
set_", Field/binary, "(Player, Data) when element(1, Data) == player_", Field/binary, " -> set(Player, #player.", Field/binary,", Data).">>.

gen_set_function_with_key(Field) ->
    <<"
set_", Field/binary,"(Player, Key, Data) when element(1, Data) == player_", Field/binary, " -> set(Player, #player.", Field/binary, ", Key, Data).">>.

gen_cache_function([{Field, _}], Bin) ->
    <<Bin/binary, (gen_cache_function(Field))/binary, ".\n\n">>;
gen_cache_function([{Field, _} | Rest], Bin) ->
    gen_cache_function(Rest, <<Bin/binary, (gen_cache_function(Field))/binary, ";\n">>).

gen_cache_function(Field) ->
    <<"cache_name(#player.", Field/binary, ") -> cache_player_", Field/binary>>.

gen_record_function([{Field, _}], Bin) ->
    <<Bin/binary, (gen_record_function(Field))/binary, ".\n\n">>;
gen_record_function([{Field, _} | Rest], Bin) ->
    gen_record_function(Rest, <<Bin/binary, (gen_record_function(Field))/binary, ";\n">>).

gen_record_function(Field) ->
    <<"record_name(cache_player_", Field/binary, ") -> player_", Field/binary>>.

gen_fields_function([{Field, _}], Bin) ->
    <<Bin/binary, (player_field(Field))/binary>>;
gen_fields_function([{Field, _} | Rest], Bin) ->
    gen_fields_function(Rest, <<Bin/binary, (player_field(Field))/binary, ", ">>).

player_field(Field) ->
    <<"#player.", (Field)/binary>>.

header() ->
    <<"%%%-------------------------------------------------------------------
%%% @doc
%%% !!! DO NOT EDIT !!!
%%% GENERATED FROM REBAR PLUGINS
%%% 放在 #player{} 中 注释 player_data 下面的数据，使用本模块进行存取
%%% 需要在 player.hrl 中定义对应字段，往后添加即可，名字尽量简短直白
%%% 会自动生成对应的 get_xxx, set_xxx接口
%%%     1. 一般key只有player_id，值为record，get返回对应record 或 undefined
%%%     2. 如果key是2个，如活动activity，{player_id, type}的形式，需要在player.hrl中定义默认值为 #{}
%%%        对应接口会多一个参数，需要传 Type，get_activity(Player, Type) -> Record.
%%% @end
%%%-------------------------------------------------------------------
-module(player).
-author(\"Jeson\").

-compile(export_all).
-compile(nowarn_export_all).

-include(\"player.hrl\").
-include(\"common.hrl\").\n\n"/utf8>>.

tail() ->
    <<"-type index() :: integer().
-type player() :: #player{}.
-type map_key() :: integer().
-type player_data() :: tuple().
-type cache_key() :: integer() | tuple().

%% @doc 用player的pos读cache数据，单key的话是 player_id, 双 key 是 {player_id, type}
-spec get_cache_data(#player{}, index(), cache_key()) -> undefined | player_data().
get_cache_data(Player, Index, Key) ->
    Cache = cache_name(Index),
    Result = cache_unit:lookup(Cache, Key),
    ?DEFAULT(Result, default_data(Player, Cache, Key)).

%% @doc 用player的pos存cache数据
-spec set_cache_data(index(), player_data()) -> ok.
set_cache_data(Index, Data) ->
    cache_unit:insert(cache_name(Index), Data).

%% @doc 读单 key 数据接口
get(#player{player_id = PlayerId, is_new = IsNew} = Player, Index) ->
    case element(Index, Player) of
        undefined when IsNew ->
            %% 新号没数据，直接返回默认值
            default_data(Player, cache_name(Index), PlayerId);
        undefined ->
            get_cache_data(Player, Index, PlayerId);
        Data -> Data
    end.

%% @doc 读双 key 数据接口
-spec get(player(), index(), map_key()) -> undefined | player_data().
get(#player{player_id = PlayerId, is_new = IsNew} = Player, Index, Type) ->
    case maps:get(Type, get(Player, Index), undefined) of
        undefined when IsNew ->
            %% 新号没数据，直接返回默认值
            default_data(Player, cache_name(Index), {PlayerId, Type});
        undefined ->
            get_cache_data(Player, Index, {PlayerId, Type});
        Data -> Data
    end.

%% @doc 存单 key 数据接口
-spec set(player(), index(), player_data()) -> player().
set(#player{player_id = PlayerId} = Player, Index, Data) when is_tuple(Data) ->
    P1 = update_dirty_keys(Player, Index),
    %% @doc 第一次把数据存在player身上的时候，判断是否需要删ets数据
    ?IF(need_delete_ets(Player, Index), ets:delete(cache_name(Index), PlayerId)),
    setelement(Index, P1, Data).

%% @doc 存双 key 数据接口
-spec set(player(), index(), map_key(), any()) -> player().
set(#player{player_id = PlayerId} = Player, Index, Key, Data) ->
    P1 = update_dirty_keys(Player, {Index, Key}),
    ?IF(need_delete_ets(Player, {Index, Key}), ets:delete(cache_name(Index), {PlayerId, Key})),
    setelement(Index, P1, (get(Player, Index))#{Key => Data}).

%% Cache 可以是cache名字或者{cache名字，Type类型}
need_data_in_ets(Player, Cache) ->
    (not player_lib:is_online(Player)) orelse need_data_in_ets_(Cache).

need_data_in_ets_(Cache) when is_atom(Cache) ->
    lists:member(Cache, player_lib:player_caches_in_ets());
need_data_in_ets_({Cache, Type}) when is_atom(Cache) ->
    lists:member(Cache, player_lib:player_caches_in_ets()) orelse 
    lists:member({Cache, Type}, player_lib:player_caches_in_ets()). 

need_update_ets(Index) when is_integer(Index)->
      need_data_in_ets_(cache_name(Index));
need_update_ets({Index, Type}) when is_integer(Index)->
      need_data_in_ets_({cache_name(Index), Type}).


need_delete_ets(Player, Index) when is_integer(Index)->
    case element(Index, Player) of
        ?UNDEF ->
            not need_data_in_ets(Player, cache_name(Index));
        _ -> false
    end;
need_delete_ets(Player,  {Index, Key}) ->
    case maps:is_key(Key, element(Index, Player)) of
        true -> false;
        false ->
            not need_data_in_ets(Player, {cache_name(Index), Key})
    end.


update_dirty_keys(Player = #player{dirty_keys = DirtyKeys}, Key) ->
    Player#player{dirty_keys = ?IF(lists:member(Key, DirtyKeys), DirtyKeys, [Key | DirtyKeys])}.


%% @doc Type :: all || only_need
%% all: 所有变化的数据更新到ets
%% only_need: 需要放在ets的数据才更新到ets
save_dirty_data_to_ets(#player{dirty_keys = []} = Player, _Type) -> {ok, Player};
save_dirty_data_to_ets(#player{dirty_keys = DirtyKeys} = Player, Type) ->
    [do_save_dirty_data_to_ets(Player, DirtyKey) || DirtyKey <- DirtyKeys,
        Type =:= all orelse need_update_ets(DirtyKey)],
    {ok, Player#player{dirty_keys = []}}.

do_save_dirty_data_to_ets(Player, Index) when is_integer(Index) ->
    Data = element(Index, Player),
    ets:insert(cache_name(Index), Data);
do_save_dirty_data_to_ets(Player, {Index, Key}) when Index == #player.task->
    %% @doc 任务比较特殊，cache_player_task 这个ets的类型是 bag，插入数据
    %% 不能直接用ets:insert
    Data = maps:get(Key, element(Index, Player)),
    cache_of_mysql:do_insert(cache_name(Index), Data);
do_save_dirty_data_to_ets(Player, {Index, Key}) ->
    Data = maps:get(Key, element(Index, Player)),
    ets:insert(cache_name(Index), Data).

save_dirty_data_to_db(#player{db_dirty_keys = []} = Player) -> {ok, Player};
save_dirty_data_to_db(#player{db_dirty_keys = DirtyKeys} = Player) ->
    [do_save_dirty_data_to_db(Player, DirtyKey) || DirtyKey <- DirtyKeys],
    {ok, Player#player{db_dirty_keys = []}}.

do_save_dirty_data_to_db(Player, Index) when is_integer(Index) ->
    Data = element(Index, Player),
    cache_unit:insert_to_db(cache_name(Index), Data);
do_save_dirty_data_to_db(Player, {Index, Key}) ->
    Data = maps:get(Key, element(Index, Player)),
    cache_unit:insert_to_db(cache_name(Index), Data).

default_data(Player, Cache, Key) ->
    Module = cache_module:cache_to_module(Cache),
    case erlang:function_exported(Module, default_data, 3) of
        true ->
            Module:default_data(Cache, Player, Key);
        false ->
            Default = record_utils:default(record_name(Cache)),
            setelement(2, Default, Key)
    end.

"/utf8>>.
