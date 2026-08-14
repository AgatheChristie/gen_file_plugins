%%%-------------------------------------------------------------------
%%% @author Jeson
%%% @copyright (C) 2023, hk
%%% @doc
%%%
%%% @end
%%% Created : 19. 10月 2023 14:28
%%%-------------------------------------------------------------------
-module(gen_record_info).
-author("Jeson").

-export([start/1, parse_record/1]).

start(_State) ->
    {RecordHeaders, Fields, Default} = scan_dir("apps/game_server/include", true),
    {ProtoHeaders, _, _} = scan_dir("apps/game/include/proto", false),
    NewFields = binary:part(Fields, 0, size(Fields) - 2),
    NewDefault = binary:part(Default, 0, size(Default) - 2),
    TarFileBin = <<(header())/binary, RecordHeaders/binary, "\n", NewFields/binary, ".\n\n", NewDefault/binary, ".">>,
    write_user_default(<<RecordHeaders/binary, ProtoHeaders/binary>>),
    ok = file:write_file("apps/game_server/src/utils/record_utils.erl", TarFileBin).

%% GenUtils=true  : include + record_utils fields/default
%% GenUtils=false : 只往 user_default 加 include
scan_dir(Dir, GenUtils) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            HrlFiles = lists:sort([F || F <- Files, filename:extension(F) =:= ".hrl"]),
            lists:foldl(fun(FileName, Acc) ->
                fold_hrl(Dir, FileName, GenUtils, Acc)
            end, {<<>>, <<>>, <<>>}, HrlFiles);
        {error, enoent} ->
            {<<>>, <<>>, <<>>}
    end.

fold_hrl(Dir, FileName, GenUtils, {XH, XF, XD}) ->
    {ok, FileBin} = file:read_file(filename:join(Dir, FileName)),
    case re:run(FileBin, <<"-record\\(.*?\\)\\.">>, [global, dotall, {capture, all, binary}]) of
        {match, L} ->
            XH1 = <<XH/binary, "-include(\"", (list_to_binary(FileName))/binary, "\").\n">>,
            case GenUtils of
                true ->
                    RecordList = [parse_record(RecordBin) || [RecordBin] <- L],
                    {XH1, gen_fields(RecordList, XF), gen_default(RecordList, XD)};
                false ->
                    {XH1, XF, XD}
            end;
        nomatch ->
            {XH, XF, XD}
    end.

write_user_default(Headers) ->
    Re = <<"%% include start.*%% include end">>,
    UserDefault = "apps/game_server/src/user_default.erl",
    ReplaceBin = <<"%% include start\n", Headers/binary, "%% include end">>,

    {ok, Bin} = file:read_file(UserDefault),
    {match, [TarBin]} = re:run(Bin, Re, [dotall, {capture, all, binary}]),
    TarBin /= ReplaceBin andalso (ok = file:write_file(UserDefault, re:replace(Bin, Re, ReplaceBin, [dotall]))).

%% @doc 解析一个record字符串 <<"-record(test, {a,b,c}).">>
%% 返回格式：{record_name, [{field_name, field_val}]}
parse_record(RecordBin) ->
    Bin1 = re:replace(RecordBin, <<"-record\\(|\\.|{|}|\s|%.*?\n|:.*?\n|\n">>, <<"">>, [global, {return ,binary}]),
    [RecordName, Rest] = binary:split(Bin1, <<",">>),
    FieldValueList = [parse_field_value(binary:split(FieldValue, <<"=">>)) || FieldValue <- binary:split(Rest, <<",">>, [global])],
    {RecordName, FieldValueList}.

parse_field_value([Field]) ->
    {Field, undefined};
parse_field_value([Field, Value]) ->
    case binary:match(Value, <<"?">>) of
        nomatch ->
            {Field, Value};
        _ ->
            {Field, undefined}
    end.

gen_fields([], Bin) ->
    Bin;
gen_fields([{Record, _} | Rest], Bin) ->
    gen_fields(Rest, <<Bin/binary, "fields(", Record/binary, ") -> record_info(fields, ", Record/binary, ");\n">>).

gen_default([], Bin) ->
    Bin;
gen_default([{Record, _} | Rest], Bin) ->
    gen_default(Rest, <<Bin/binary, "default(", Record/binary, ") -> #", Record/binary, "{};\n">>).

header() ->
    <<"%%%-------------------------------------------------------------------
%%% @doc
%%% !!! DO NOT EDIT !!!
%%% GENERATED FROM REBAR PLUGINS
%%% 自动生成 record_info 和 default_record
%%% @end
%%%-------------------------------------------------------------------

-module(record_utils).

-compile(export_all).
-compile(nowarn_export_all).\n\n"/utf8>>.
