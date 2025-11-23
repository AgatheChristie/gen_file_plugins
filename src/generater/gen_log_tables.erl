%%%-------------------------------------------------------------------
%%% @author taiqi
%%% @copyright (C) 2025, hk
%%% @doc
%%%
%%% @end
%%% Created : 21. 11月 2025 11:12
%%%-------------------------------------------------------------------
-module(gen_log_tables).

-include("cache_map.hrl").

-export([start/0]).


-define(CVGENIF(TrueOrFalse, A, B),
    case TrueOrFalse of
        true -> A;
        false -> B
    end
).


-define(CVGENDEFAULT(_Data, _Default),     (
        case _Data of
            undefined ->
                _Default;
            false ->
                _Default;
            error ->
                _Default;
            _RetData ->
                _RetData
        end
)).


-define(SQL_DIR,                        "sql/").
-define(INCLUDE_DIR,                    "apps/game_server/include/").
-define(TABLES_DIR,                     "apps/game_server/src/game_log/").
-define(LOG_TABLES,                     ?TABLES_DIR ++ "log_tables.erl").
-define(GAME_TABLES,                    ?TABLES_DIR ++ "game_tables.erl").
-define(COMPILE_PATH,                   "_build/default/plugins/gen_file/").
-define(COMPILE_OPTS,                   [
    debug_info, report, {outdir, ?COMPILE_PATH},
    {i, "apps/memcache/include/"},
    {i, "apps/cc_data/src/server/hrl/base/"},
    {i, "apps/game_server/include/"},
    {i, "_build/default/plugins/game/include/"},
    {i, "_build/default/plugins/gen_file/include/"}]).

start() ->
   CompileTime = gen_file:get_compile_time(),
   true = code:add_path(?COMPILE_PATH),
   MTime1 = gen_file:get_modify_time(?LOG_TABLES),
   (MTime1 == 0 orelse MTime1 > CompileTime) andalso gen_log_tables(),
    MTime2 = gen_file:get_modify_time(?GAME_TABLES),
   (gen_assets_file:is_assets_data_changed() orelse MTime2 == 0 orelse MTime2 > CompileTime) andalso parse_game_tables(),
    ok.

gen_log_tables() ->
    compile_and_load_beam(?LOG_TABLES, log_tables),

    TabList = log_tables:get(gen_assets_file:all_assets_and_name()),
    #{sql := SqlBin, rec := RecList} = parse_db_tables(TabList, #{sql => <<>>, rec => []}),
    HeaderFile = ?INCLUDE_DIR ++ "game_log.hrl",
    ok = file:write_file(HeaderFile, binary:list_to_bin([element(4, X) || X <- RecList])),
    not filelib:is_dir(?SQL_DIR) andalso file:make_dir(?SQL_DIR),
    ok = file:write_file(?SQL_DIR ++ "game_log.sql", SqlBin),

    FileBin = gen_game_log_lib(TabList, header()),
    ok = file:write_file(?TABLES_DIR ++ "game_log_lib.erl", FileBin).

gen_game_log_lib([], Bin) ->
    Bin;
gen_game_log_lib([#db_table{} = Tab | Rest], Bin) ->
    FunBin = gen_log_fun(Tab),
    gen_game_log_lib(Rest, <<Bin/binary, FunBin/binary>>).

gen_log_fun(#db_table{name = TabName, fields = Fields, comment = Comment}) ->
    TabNameBin = atom_to_binary(TabName),
    {FieldBin1, FieldBin2} = gen_log_field(Fields, <<>>, <<>>, <<>>, 0),
    <<"%% @doc ", Comment/binary, "\n", TabNameBin/binary,
        FieldBin1/binary, "    Record = #", TabNameBin/binary,
        "{\n", FieldBin2/binary, "    },\n    write_log(", TabNameBin/binary, ", Record).\n\n">>.

-define(S8,                                     <<"        ">>).
gen_log_field([], Bin1, Bin2, Bin3, _) ->
    Bin4 = binary:part(Bin1, 2, size(Bin1) - 2),
    Bin6 = binary:replace(Bin3, <<", ">>, <<>>),
    {<<"(", Bin4/binary, ") -> \n", Bin2/binary>>, Bin6};
gen_log_field([#db_field{increment = true} | Rest], Bin1, Bin2, Bin3, HC) ->
    gen_log_field(Rest, Bin1, Bin2, Bin3, HC);
gen_log_field([#db_field{name = time} | Rest], Bin1, Bin2, Bin3, HC) ->
    Bin6 = <<Bin3/binary, (?S8)/binary, ", time = date_utils:unixtime()\n">>,
    gen_log_field(Rest, Bin1, Bin2, Bin6, HC);
gen_log_field([#db_field{name = server_id} | Rest], Bin1, Bin2, Bin3, HC) ->
    Bin6 = <<Bin3/binary, (?S8)/binary, ", server_id = sys_utils:server_id()\n">>,
    gen_log_field(Rest, Bin1, Bin2, Bin6, HC);
gen_log_field([#db_field{name = source}, #db_field{name = source_param} | Rest], Bin1, Bin2, Bin3, HC) ->
    Bin4 = <<Bin1/binary, ", Source">>,
    Bin5 = <<Bin2/binary, "    {Source1, SourceParam} = source(Source),\n">>,
    Bin6 = <<Bin3/binary,
        (?S8)/binary, ", source = Source1\n",
        (?S8)/binary, ", source_param = SourceParam\n">>,
    gen_log_field(Rest, Bin4, Bin5, Bin6, HC);
gen_log_field([
    #db_field{name = player_id}, #db_field{name = p_account},
    #db_field{name = p_level}, #db_field{name = p_name}, #db_field{name = p_channel} | Rest], Bin1, Bin2, Bin3, 0) ->
    Bin4 = <<Bin1/binary, ", PlayerHead">>,
    Bin5 = <<Bin2/binary, "    #{player_id := PlayerId, account := PAccount, name := PName, sChannel := PChannel, level := PLevel} = head(PlayerHead),\n">>,
    Bin6 = <<Bin3/binary,
        (?S8)/binary, ", player_id = PlayerId\n",
        (?S8)/binary, ", p_account = PAccount\n",
        (?S8)/binary, ", p_level = PLevel\n",
        (?S8)/binary, ", p_channel = PChannel\n",
        (?S8)/binary, ", p_name = PName\n" >>,
    gen_log_field(Rest, Bin4, Bin5, Bin6, 1);
gen_log_field([
    #db_field{name = player_id}, #db_field{name = p_account},
    #db_field{name = p_level}, #db_field{name = p_name}, #db_field{name = p_channel}  | Rest], Bin1, Bin2, Bin3, HC) ->
    HCBin = integer_to_binary(HC),
    Bin4 = <<Bin1/binary, ", PlayerHead", HCBin/binary>>,
    Bin5 = <<Bin2/binary, "    #{player_id := PlayerId", HCBin/binary,
        ", account := PAccount", HCBin/binary,
        ", name := PName", HCBin/binary, ", sChannel := PChannel", HCBin/binary, ", level := PLevel", HCBin/binary,"} = head(PlayerHead", HCBin/binary, "),\n">>,
    Bin6 = <<Bin3/binary,
        (?S8)/binary, ", player_id", HCBin/binary, " = PlayerId", HCBin/binary, "\n",
        (?S8)/binary, ", p_account", HCBin/binary, " = PAccount", HCBin/binary, "\n",
        (?S8)/binary, ", p_level", HCBin/binary, " = PLevel", HCBin/binary, "\n",
        (?S8)/binary, ", p_channel", HCBin/binary, " = PChannel", HCBin/binary, "\n",
        (?S8)/binary, ", p_name", HCBin/binary, " = PName", HCBin/binary, "\n">>,
    gen_log_field(Rest, Bin4, Bin5, Bin6, HC + 1);
gen_log_field([#db_field{name = Field, type = Type} | Rest], Bin1, Bin2, Bin3, HC)
    when Type == term; Type == text; Type == varchar ->
    FieldBin = atom_to_binary(Field),
    CamelField = camel_case(FieldBin),
    Bin4 = <<Bin1/binary, ", ", CamelField/binary>>,
    Bin6 = <<Bin3/binary, (?S8)/binary, ", ", FieldBin/binary,
        " = to_binary(", CamelField/binary, ")\n">>,
    gen_log_field(Rest, Bin4, Bin2, Bin6, HC);
gen_log_field([#db_field{name = Field} | Rest], Bin1, Bin2, Bin3, HC) ->
    FieldBin = atom_to_binary(Field),
    CamelField = camel_case(FieldBin),
    Bin4 = <<Bin1/binary, ", ", CamelField/binary>>,
    Bin6 = <<Bin3/binary, (?S8)/binary, ", ", FieldBin/binary, " = ", CamelField/binary, "\n">>,
    gen_log_field(Rest, Bin4, Bin2, Bin6, HC).

parse_game_tables() ->
    compile_and_load_beam(?GAME_TABLES, game_tables),

    TabList = game_tables:get(),
    AssetsSql = gen_assets_file:gen_assets_sql(),
    #{sql := SqlBin, rec := RecList} = parse_db_tables(TabList, #{sql => AssetsSql, rec => []}),
    [write_header(Tab, Header, Re, RecBin) || {Tab, Header, Re, RecBin} <- RecList],
    ok = file:write_file(?SQL_DIR ++ "game_server.sql", SqlBin).

compile_and_load_beam(File, Mod) ->
    {ok, _} = compile:file(File, ?COMPILE_OPTS),
    ok = code:atomic_load([Mod]).

write_header(Tab, undefined, Re, RecBin) ->
    write_header(Tab, Tab, Re, RecBin);
write_header(_Tab, Header, Re, RecBin) ->
    HeaderFile = ?INCLUDE_DIR ++ atom_to_list(Header) ++ ".hrl",
    {ok, FileBin} =
        case filelib:is_file(HeaderFile) of
            true ->
                file:read_file(HeaderFile);
            false ->
                {ok, <<>>}
        end,

    Bin1 = re:replace(FileBin, Re, <<>>, [dotall, {return, binary}]),
    ok = file:write_file(HeaderFile, <<Bin1/binary, RecBin/binary>>).

parse_db_tables([], #{rec := RecList} = Args) ->
    Args#{rec => lists:reverse(RecList)};
parse_db_tables([Tab | Rest], #{sql := Sql, rec := List} = Args) ->
    Sql1 = <<Sql/binary, (create_sql(Tab))/binary>>,
    parse_db_tables(Rest, Args#{sql => Sql1, rec => gen_rec(Tab, List)}).

gen_rec(#db_table{gen_rec = false}, List) ->
    List;
gen_rec(#db_table{name = Name, header = Header, fields = Fields}, List) ->
    Upper = string:uppercase(atom_to_list(Name)),
    CommentBin = <<"%% 此结构为自动生成，禁止修改\n"/utf8>>,
    RecDefine = <<"REC_", (list_to_binary(Upper))/binary>>,
    RecBin = <<CommentBin/binary, "-record(", (atom_to_binary(Name))/binary, ", {\n">>,
    Head = <<"\n\n-ifndef(", RecDefine/binary, ").\n-define(", RecDefine/binary, ", 1).\n">>,
    RecordBin = <<Head/binary, (gen_filed_bin(Fields, RecBin, 0))/binary, "}).\n-endif.\n">>,
    [{Name, Header, <<"\n\n-ifndef\\(", RecDefine/binary, "\\).*?endif\\.\n">>, RecordBin} | List].

-define(MAX_RECORD_STR_LEN,                                 42).
gen_filed_bin([], Bin, _) ->
    {match, [_, S1, S2]} = re:run(Bin, "(.*),(\s+%\s.*?\n)", [dotall, {capture, all, binary}]),
    <<S1/binary, " ", S2/binary>>;
gen_filed_bin([#db_field{increment = true} | Rest], Bin, HC) ->
    gen_filed_bin(Rest, Bin, HC);
gen_filed_bin([#db_field{type = Type, name = Name, default = Default, comment = Comment} | Rest], Bin, HC) ->
    CommentBin = parse_comment(Comment, Name),
    CommentBin1 = binary:part(CommentBin, 8, size(CommentBin) - 8),
    DefaultBin = parse_field_default(Type, Default),
    {HC1, CntBin} = get_count_bin(Name, HC),
    FieldNameBin = <<(atom_to_binary(Name))/binary, CntBin/binary>>,

    FieldBin = <<"    ", FieldNameBin/binary, " = ", DefaultBin/binary, ",">>,
    SpaceLen = max(1, ?MAX_RECORD_STR_LEN - size(FieldBin)),
    SpaceBin = binary:list_to_bin(lists:duplicate(SpaceLen, <<" ">>)),
    gen_filed_bin(Rest, <<Bin/binary, FieldBin/binary, SpaceBin/binary, "%", CommentBin1/binary, "\n">>, HC1).

parse_field_default(Int, Default) when Int == bigint orelse Int == int orelse Int == tinyint ->
    integer_to_binary(?CVGENIF(is_integer(Default), Default, 0));
parse_field_default(term, Default) ->
    to_binary(Default);
parse_field_default(Str, Default) when Str == varchar orelse Str == text ->
    to_binary(?CVGENDEFAULT(Default, <<>>));
parse_field_default(blob, _) ->
    <<"blob">>;
parse_field_default(decimal, _) ->
    <<"0.00">>.

create_sql(#db_table{gen_sql = false}) ->
    <<>>;
create_sql(#db_table{name = TabName, fields = Fields, index_list = IndexList, comment = Comment}) ->
    CommentBin = parse_comment(Comment, TabName),
    TabNameBin = atom_to_binary(TabName),
    FieldsBin = gen_field_sql(Fields, <<>>, 0),
    IndexBin = gen_index_sql(IndexList, [N || #db_field{name = N} <- Fields], <<>>),
    NewIndexBin = binary:part(IndexBin, 0, size(IndexBin) - 2),
    <<"CREATE TABLE IF NOT EXISTS `", TabNameBin/binary, "` (\n",
        FieldsBin/binary, NewIndexBin/binary, "\n) ENGINE=InnoDB DEFAULT ",
        "CHARSET=utf8mb4 COLLATE utf8mb4_bin", CommentBin/binary, ";\n\n">>.

gen_index_sql([], _, Bin) ->
    Bin;
gen_index_sql([#db_index{name = RawName, fields = RawFields} = Index | Rest], FieldName, Bin) ->
    (RawFields -- FieldName) /= [] andalso throw({field_error, RawName, RawFields}),
    #db_index{type = Type, name = Name, fields = Fields} = parse_index(Index),
    gen_index_sql(Rest, FieldName, <<Bin/binary, "    ", Type/binary, Name/binary, Fields/binary, ",\n">>).

parse_index(#db_index{type = primary, fields = Fields} = Index) when length(Fields) > 0->
    parse_index_data(Index#db_index{name = "", type = <<"PRIMARY KEY">>});
parse_index(#db_index{type = normal, name = Name, fields = Fields} = Index)
    when is_list(Name) andalso length(Fields) > 0 ->
    parse_index_data(Index#db_index{type = <<"KEY">>});
parse_index(#db_index{type = unique, name = Name, fields = Fields} = Index)
    when is_list(Name) andalso length(Fields) > 0 ->
    parse_index_data(Index#db_index{type = <<"UNIQUE KEY">>}).

parse_index_data(#db_index{name = Name, fields = Fields} = Index) ->
    FieldList = ["`" ++ atom_to_list(Field) ++ "`" || Field <- Fields],
    FieldsBin = <<" (", (list_to_binary(string:join(FieldList, ", ")))/binary, ")">>,
    NameBin = parse_field(Name),
    NewNameBin = ?CVGENIF(NameBin == <<>>, <<>>, <<" ", NameBin/binary>>),
    Index#db_index{name = NewNameBin, fields = FieldsBin}.

gen_field_sql([], Bin, _HC) ->
    Bin;
gen_field_sql([#db_field{ignore = true} | Rest], Bin, HC) ->
    gen_field_sql(Rest, Bin, HC);
gen_field_sql([#db_field{name = Name} = Field | Rest], Bin, HC) ->
    #db_field{type = Type, default = Default, increment = Increment,
        comment = Comment, unsigned = Unsigned} = parse_db_field(Field),
    {HC1, HCBin} = get_count_bin(Name, HC),
    FieldName = <<(atom_to_binary(Name))/binary, HCBin/binary>>,
    gen_field_sql(Rest, <<Bin/binary, "    ", (parse_field(FieldName))/binary, " ",
        Type/binary, Unsigned/binary, Default/binary, Increment/binary, Comment/binary, ",\n">>, HC1).

get_count_bin(p_name, 0) ->
    {1, <<>>};
get_count_bin(p_name, Cnt) ->
    {Cnt + 1, <<(integer_to_binary(Cnt))/binary>>};
get_count_bin(Name, Cnt) when (Name == p_account orelse Name == player_id orelse Name == p_level) andalso Cnt > 0 ->
    {Cnt, <<(integer_to_binary(Cnt))/binary>>};
get_count_bin(_, Cnt) ->
    {Cnt, <<>>}.

-define(DEFAULT_VARCAHR_LEN,                        64).
%% @doc 解析 #db_field 字段 这里会把 type default 转成对应 binary
parse_db_field(#db_field{type = term, len = Len} = Field) ->
    parse_db_field(Field#db_field{type = varchar, len = ?CVGENDEFAULT(Len, 255)});
parse_db_field(#db_field{type = int, default = Default, increment = Inc} = Field)
    when is_integer(Default) orelse Default == undefined ->
    DefaultBin = ?CVGENIF(Inc, undefined, integer_to_binary(?CVGENDEFAULT(Default, 0))),
    parse_desc_data(Field#db_field{type = <<"int(10)">>, default = DefaultBin});
parse_db_field(#db_field{type = tinyint, default = Default, increment = Inc} = Field)
    when is_integer(Default) orelse Default == undefined ->
    DefaultBin = ?CVGENIF(Inc, undefined, integer_to_binary(?CVGENDEFAULT(Default, 0))),
    parse_desc_data(Field#db_field{type = <<"tinyint(3)">>, default = DefaultBin});
parse_db_field(#db_field{type = bigint, default = Default, increment = Inc} = Field)
    when is_integer(Default) orelse Default == undefined ->
    DefaultBin = ?CVGENIF(Inc, undefined, integer_to_binary(?CVGENDEFAULT(Default, 0))),
    parse_desc_data(Field#db_field{type = <<"bigint(20)">>, default = DefaultBin});
parse_db_field(#db_field{type = datetime = Type} = Field) ->
    parse_desc_data(Field#db_field{type = atom_to_binary(Type), default = undefined, unsigned = false, increment = false});
parse_db_field(#db_field{type = text = Type} = Field) ->
    parse_desc_data(Field#db_field{type = atom_to_binary(Type), unsigned = false, increment = false});
parse_db_field(#db_field{type = varchar = Type, len = Len, default = Default} = Field)
    when (is_integer(Len) andalso Len > 0) orelse Len == undefined ->
    Len1 = ?CVGENIF(is_integer(Len), Len ,?DEFAULT_VARCAHR_LEN),
    TypeBin = <<(atom_to_binary(Type))/binary, "(", (integer_to_binary(Len1))/binary, ")">>,
    parse_desc_data(Field#db_field{type = TypeBin, default = ?CVGENIF(Default == undefined, <<>>, to_binary(Default)), unsigned = false, increment = false});
parse_db_field(#db_field{type = blob = Type, default = undefined} = Field) ->
    parse_desc_data(Field#db_field{type = atom_to_binary(Type), unsigned = false, increment = false});
parse_db_field(#db_field{type = decimal = Type, len = {M, D}, default = Default} = Field) when is_float(Default) orelse Default == undefined ->
    DefaultBin = ?CVGENIF(is_float(Default), float_to_binary(Default), <<"0.00">>),
    TypeBin = <<(atom_to_binary(Type))/binary, "(", (integer_to_binary(M))/binary, ",", (integer_to_binary(D))/binary, ")">>,
    parse_desc_data(Field#db_field{type = TypeBin, default = DefaultBin, unsigned = false, increment = false}).

%% @doc 将 #db_field 所有字段转换成对应的 sql_binary
parse_desc_data(#db_field{
    name = Name, increment = Increment, type = Type,
    default = Default, comment = Comment, unsigned = Unsigned} = Field) ->
    Field#db_field{
        default = parse_default(Type, Default), comment = parse_comment(Comment, Name),
        unsigned = parse_unsigned(Unsigned), increment = parse_increment(Increment)}.

parse_increment(false) ->
    <<>>;
parse_increment(true) ->
    <<" AUTO_INCREMENT">>.

parse_unsigned(true) ->
    <<" unsigned">>;
parse_unsigned(false) ->
    <<>>.

parse_field([]) ->
    <<>>;
parse_field(Field) when is_list(Field) ->
    parse_field(list_to_binary(Field));
parse_field(Field) when is_atom(Field) ->
    parse_field(atom_to_binary(Field));
parse_field(Field) when is_binary(Field) ->
    <<"`", Field/binary, "`">>.

parse_comment(undefined, Name) ->
    parse_comment(atom_to_binary(Name), undefined);
parse_comment(Comment, _) when is_list(Comment) ->
    parse_comment(list_to_binary(Comment), undefined);
parse_comment(Comment, _) when is_binary(Comment) ->
    <<" COMMENT \'", Comment/binary, "\'">>.

parse_default(Type, _)
    when Type == <<"text">>; Type == <<"blob">> ->
    <<" NOT NULL">>;
parse_default(_, undefined) ->
    <<" NOT NULL">>;
parse_default(_, Default) when is_binary(Default) ->
    <<" NOT NULL DEFAULT '", Default/binary, "'">>.

to_binary(Term) ->
    unicode:characters_to_binary(io_lib:format("~w", [Term])).

header() ->
    <<"%%%-------------------------------------------------------------------
%%% @doc
%%% !!! DO NOT EDIT !!!
%%% GENERATED FROM REBAR PLUGINS
%%% 自动生成 log 相关接口
%%% @end
%%%-------------------------------------------------------------------

-module(game_log_lib).

-compile(export_all).
-compile(nowarn_export_all).

-include(\"game_log.hrl\").

write_log(Tab, Record) ->
    gen_server:cast(Tab, {log, Record}).

head(Head) when is_map(Head) ->
    Head;
head(Player) when element(1, Player) == player ->
    player_head:log_head(Player).

to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(Data) ->
    common_utils:term_to_binary(Data).

source(Source) when is_integer(Source) ->
    {Source, <<\"[]\">>};
source([Source | SourceParam]) ->
    {Source, to_binary(SourceParam)}.

"/utf8>>.

-define(LETTER_DIFF,                ($a - $A)).

camel_case(Binary) when is_binary(Binary) ->
    camel_case(<<"_", Binary/binary>>, <<>>).

camel_case(<<>>, Bin) ->
    Bin;
camel_case(<<"_", C:8, Rest/binary>>, Bin) ->
    camel_case(Rest, <<Bin/binary, (C - ?LETTER_DIFF):8>>);
camel_case(<<C:8, Rest/binary>>, Bin) ->
    camel_case(Rest, <<Bin/binary, C:8>>).
