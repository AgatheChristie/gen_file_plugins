%%%-------------------------------------------------------------------
%%% @author taiqi
%%% @copyright (C) 2025, xhm
%%% @doc
%%%
%%% @end
%%% Created : 19. 7月 2025 13:28
%%%-------------------------------------------------------------------
-module(gen_player_erl).


-export([start/0, tail/0, header/0, gen_cache_function/2, gen_fields_function/2, gen_player_function/2, parse_player_fields/0]).
start() -> ok.
%%% start() ->
%%%    Fields = parse_player_fields(),
%%%    FunBin = gen_player_function(Fields, <<>>),
%%%    CacheBin = gen_cache_function(Fields, <<>>),
 %%%   FieldsBin = gen_fields_function(Fields, <<>>),
 %%%   FieldsBin1 = <<"all_fields() ->\n    [", FieldsBin/binary, "].\n\n">>,
 %%%   FileBin = <<(header())/binary, FunBin/binary, "\n", CacheBin/binary, FieldsBin1/binary, (tail())/binary>>,
 %%%   ok = file:write_file("apps/game_server/src/player/player.erl", FileBin).

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
gen_player_function([{Field = <<"base">>, undefined} | Rest], Bin) ->
    GetFunBin = gen_get_function(Field),
    SetFunBin = gen_set_function(Field),
    SetFunWithCacheBin = gen_set_function_with_cache(Field),
    Bin1 = <<Bin/binary, GetFunBin/binary, SetFunBin/binary, SetFunWithCacheBin/binary, "\n">>,
    gen_player_function(Rest, Bin1);
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
get_", Field/binary, "(Player) -> get_", Field/binary, "(Player, undefined).
get_", Field/binary, "(Player, Default) -> ?DEFAULT(get(Player, #player.", Field/binary, "), Default).">>.

gen_get_function_with_key(Field) ->
    <<"
get_", Field/binary, "(Player, Key) -> get_", Field/binary, "(Player, Key, undefined).
get_", Field/binary, "(Player, Key, Default) -> ?DEFAULT(get(Player, #player.", Field/binary, ", Key), Default).">>.

gen_set_function(Field) ->
   <<"
set_", Field/binary, "(Player, Data) when element(1, Data) == player_", Field/binary, " -> set(Player, #player.", Field/binary,", Data).">>.

gen_set_function_with_cache(Field) ->
    <<"
set_", Field/binary, "_with_cache(Player, Data) when element(1, Data) == player_", Field/binary, " ->
    set_cache_data(#player.", Field/binary, ", Data),
    set(Player, #player.", Field/binary,", Data).">>.

gen_set_function_with_key(Field) ->
    <<"
set_", Field/binary,"(Player, Key, Data) when element(1, Data) == player_", Field/binary, " -> set(Player, #player.", Field/binary, ", Key, Data).">>.

gen_cache_function([{Field, _}], Bin) ->
    <<Bin/binary, (gen_cache_function(Field))/binary, ".\n\n">>;
gen_cache_function([{Field, _} | Rest], Bin) ->
    gen_cache_function(Rest, <<Bin/binary, (gen_cache_function(Field))/binary, ";\n">>).

gen_cache_function(Field) ->
    <<"cache_name(#player.", Field/binary, ") -> cache_player_", Field/binary>>.

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
-spec get_cache_data(index(), cache_key()) -> undefined | player_data().
get_cache_data(Index, Key) ->
    cache_unit:lookup(cache_name(Index), Key).

%% @doc 用player的pos存cache数据
-spec set_cache_data(index(), player_data()) -> ok.
set_cache_data(Index, Data) ->
    cache_unit:insert(cache_name(Index), Data).

%% @doc 读单 key 数据接口
get(#player{player_id = PlayerId, is_new = IsNew} = Player, Index) ->
    case element(Index, Player) of
        undefined ->
            ?IF(IsNew, undefined, get_cache_data(Index, PlayerId));
        Data -> Data
    end.

%% @doc 读双 key 数据接口
-spec get(player(), index(), map_key()) -> undefined | player_data().
get(#player{player_id = PlayerId, is_new = IsNew} = Player, Index, Key) ->
    case maps:get(Key, get(Player, Index), undefined) of
        undefined ->
            ?IF(IsNew, undefined, get_cache_data(Index, {PlayerId, Key}));
        Data -> Data
    end.

%% @doc 存单 key 数据接口
-spec set(player(), index(), player_data()) -> player().
set(#player{} = Player, Index, Data) when is_tuple(Data) ->
    P1 = update_dirty_keys(Player, Index),
    setelement(Index, P1, Data).

%% @doc 存双 key 数据接口
-spec set(player(), index(), map_key(), any()) -> player().
set(#player{} = Player, Index, Key, Data) ->
    P1 = update_dirty_keys(Player, {Index, Key}),
    setelement(Index, P1, (get(Player, Index))#{Key => Data}).

update_dirty_keys(Player = #player{dirty_keys = DirtyKeys}, Key) ->
    Player#player{dirty_keys = ?IF(lists:member(Key, DirtyKeys), DirtyKeys, [Key | DirtyKeys])}.

%% @doc 保存 #player{} 的数据到 cache后面的数据
save_dirty_data(#player{dirty_keys = []} = Player) -> {ok, Player};
save_dirty_data(#player{dirty_keys = DirtyKeys} = Player) ->
    [do_save_dirty_data(Player, DirtyKey) || DirtyKey <- DirtyKeys],
    {ok, Player#player{dirty_keys = []}}.

do_save_dirty_data(Player, Index) when is_integer(Index) ->
    Data = element(Index, Player),
    set_cache_data(Index, Data);
do_save_dirty_data(Player, {Index, Key}) ->
    Data = maps:get(Key, element(Index, Player)),
    set_cache_data(Index, Data).

%% @doc 把player的所有数据更到cache
sync_player_to_cache(Player) ->
    lists:foreach(
        fun(Index) ->
            case element(Index, Player) of
                undefined -> ok;
                Data when is_map(Data) ->
                    [set_cache_data(Index, Value) || Value <- maps:values(Data)];
                Data ->
                    set_cache_data(Index, Data)
            end
        end, all_fields()
    ),
    {ok, Player#player{dirty_keys = []}}.

"/utf8>>.
