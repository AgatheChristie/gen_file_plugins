%%%-------------------------------------------------------------------
%%% @author
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 11月 2023 19:44
%%%-------------------------------------------------------------------
-module(gen_assets_file).
-author("panhao").

%% API
-export([start/1, gen_assets_sql/0, all_assets_and_name/0, is_assets_data_changed/0]).

start(_State) ->
    case is_assets_data_changed() of
        true ->
            {Assets, Names} = all_assets_and_name(),
            AssetsInfo = lists:zip(Assets, Names),

            %% @doc 生成 assets.hrl
            FileBin = <<(header())/binary, (gen_record_fields(AssetsInfo))/binary, (player_assets_end())/binary,
                (gen_assets_event(Assets))/binary, (file_end())/binary>>,
            ok = file:write_file("apps/game_server/include/assets.hrl", FileBin);

%%             @doc 修改game_log.sql
%%            catch write_player_assets_sql(AssetsInfo),
%%            ok;
        false ->
            ok
    end.

all_assets_and_name() ->
    {ok, Bin} = file:read_file("apps/game_server/src/data/assets_data.erl"),
    RE = <<".*all_name\\(\\) -> \\[(.*)\\].">>,
    {match, [_, AssetsStr]} = re:run(Bin, RE,[{capture, all, list}]),
    Assets = [string:trim(S) || S <- string:split(AssetsStr, ",", all)],

    NameRE = <<".*all_show_name\\(\\) -> \\[(.*)\\].">>,
    {match, [_, NameStr]} = re:run(Bin, NameRE,[{capture, all, binary}]),
    Names = [begin S1 = string:trim(S), string:trim(S1, both, [$?, $T, $(, $), $"])  end
        || S <- string:split(NameStr, ",", all)],
    {Assets, Names}.




header() ->
    <<"%%%-------------------------------------------------------------------
%%% @doc
%%% !!! DO NOT EDIT !!!
%%% GENERATED FROM REBAR PLUGINS
%%% 编译的时候根据assets_data自动生成
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ASSETS_H_H_).
-define(ASSETS_H_H_, 1).

-record(player_assets, {
    player_id = 0,
    version = 0,
"/utf8>>.

player_assets_end() ->
    <<"
    })."/utf8>>.

file_end() ->
    <<"


-endif.
    ">>.


gen_record_fields(AssetsInfo) ->
    FieldStrList = gen_fields_str_list(AssetsInfo, []),
    list_to_binary(string:join(FieldStrList, "\n")).

gen_fields_str_list([], FieldStrList) -> lists:reverse(FieldStrList);
gen_fields_str_list([{Field, Name}], FieldStrList) ->
    FieldStr = "    " ++ Field ++ " = 0                %% " ++ binary_to_list(Name),
    gen_fields_str_list([], [FieldStr | FieldStrList]);
gen_fields_str_list([{Field, Name} | Tail], FieldStrList) ->
    FieldStr = "    " ++ Field ++ " = 0,               %% " ++ binary_to_list(Name),
    gen_fields_str_list(Tail, [FieldStr | FieldStrList]).

%% @doc 资产的事件固定从200开始
gen_assets_event(AllFields) ->
    {EventList, _} =
        lists:foldl(
            fun(Assets, {TmpList, EventId}) ->
                AddEvent = "-define(EVENT_" ++ string:uppercase(Assets) ++ "_ADD,    " ++ integer_to_list(EventId) ++ ").",
                ReduceEvent = "-define(EVENT_" ++ string:uppercase(Assets) ++ "_REDUCE,    "
                    ++ integer_to_list(EventId + 1) ++ ").",
                {[ReduceEvent, AddEvent | TmpList], EventId + 2}
            end, {[], 201}, AllFields
            ),
    list_to_binary("\n\n" ++ string:join(lists:reverse(EventList), "\n")).

is_assets_data_changed() ->
    gen_file:get_modify_time("apps/game_server/src/data/assets_data.erl") > gen_file:get_compile_time().
%%
%%write_player_assets_sql(AssetsInfo) ->
%%    Sql = gen_sql(AssetsInfo),
%%    Re = <<"--  player_assets_begin.*--  player_assets_end">>,
%%    UserDefault = "sql/game_server.sql",
%%    ReplaceBin = <<"--  player_assets_begin\n", Sql/binary, "\n--  player_assets_end">>,
%%
%%    {ok, Bin} = file:read_file(UserDefault),
%%    ok = file:write_file(UserDefault, re:replace(Bin, Re, ReplaceBin, [dotall])).

gen_assets_sql() ->
    {Assets, Names} = all_assets_and_name(),
    gen_sql(lists:zip(Assets, Names)).

gen_sql(AssetsInfo) ->
    Head = <<"CREATE TABLE IF NOT EXISTS `player_assets` (
  `player_id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '角色ID',
  `version` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '版本号',
"/utf8>>,
    FieldStrList = [ "  `" ++ Field ++ "` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '"  ++ binary_to_list(Name) ++ "',"
        || {Field, Name} <- AssetsInfo],
    FieldStr = list_to_binary(string:join(FieldStrList, "\n")),
    End = <<"
  PRIMARY KEY (`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE utf8mb4_bin COMMENT='角色货币资产表';\n\n"/utf8>>,
    <<Head/binary, FieldStr/binary, End/binary>>.


