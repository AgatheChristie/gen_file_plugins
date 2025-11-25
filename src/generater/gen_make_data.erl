-module(gen_make_data).

-export([start/1, do/1, format_error/1]).

-include("px_plugin.hrl").

-include_lib("kernel/include/file.hrl").


start(State) ->
    do(State).

do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
%%    {Args, _} = rebar_state:command_parsed_args(State),
    Lang = "zh",
    make_data(Apps, Lang),
    ok.


make_data([AppInfo | Tail], Lang) ->
    Opts = rebar_app_info:opts(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    ErlOpts = rebar_opts:get(Opts, erl_opts),
    case rebar_opts:get(Opts, data_opts, ?NONE) of
        ?NONE -> ok;
        DataOpts ->
            ?LOG_INFO("awdawd for app ~s", [rebar_app_info:name(AppInfo)]),
            DataDir = proplists:get_value(data_dir, DataOpts, "data"),
            FromDataDir = filename:join([AppDir, DataDir ++ "_" ++ Lang]),
            ToDataDir = filename:join([AppDir, DataDir]),
            plugin_util:ensure_dir(ToDataDir),
            DataIndexFile = proplists:get_value(data_index_file, DataOpts, "src/util/data_index.erl"),
            DataIndexPath = filename:join(AppDir, DataIndexFile),
            code:add_pathz(EbinDir),
            {ok, DataIndexMod} = make_data_index_module(DataIndexPath, EbinDir, ErlOpts),
            sync_config_files(FromDataDir, ToDataDir, DataIndexMod),
            FirstList = DataIndexMod:custom_first(),
            DirList = [{app_dir, AppDir}, {out_dir, EbinDir}, {from_data_dir, FromDataDir}, {to_data_dir, ToDataDir}],
            create_custom(FirstList, DirList, ErlOpts),
            CustomList = DataIndexMod:custom(),
            create_custom(CustomList, DirList, ErlOpts)
    end,
    make_data(Tail, Lang);
make_data([], _) ->
    ok.

make_data_index_module(DataIndexPath, EbinDir, ErlOpts) ->
    case compile:file(DataIndexPath, ErlOpts ++ [binary, return]) of
        {ok, _, EBin, _Warn} ->
            ?LOG_DEBUG("Rebuild ~s success", [DataIndexPath]),
            BaseName = filename:basename(DataIndexPath, ".erl"),
            Mod = plugin_util:to_atom(BaseName),
            EbinFile = filename:join([EbinDir, BaseName ++ ".beam"]),
            code:load_binary(Mod, EbinFile, EBin),
            {ok, Mod};
        Ret ->
            rebar_api:abort("rebuild ~s error, ~nRet:~p", [DataIndexPath, Ret])
    end.

sync_config_files(FromDataDir, ToDataDir, DataIndexMod) ->
    DataFileSets = ordsets:from_list(filelib:wildcard("*.erl", ToDataDir)),
    FromFileSets = ordsets:from_list(filelib:wildcard("*.erl", FromDataDir)),
    DelFileSets = ordsets:subtract(DataFileSets, FromDataDir),
    IndexModSets = ordsets:from_list(DataIndexMod:list_config()),
    [file:delete(filename:join(ToDataDir, DelFile)) || DelFile <- ordsets:to_list(DelFileSets)],
    CopyFiles = lists:filter(
        fun(FileName) ->
            Mod = plugin_util:to_atom(filename:basename(FileName, ".erl")),
            ordsets:is_element(Mod, IndexModSets) == false
        end, ordsets:to_list(FromFileSets)
    ),
    rebar_parallel:queue(
        CopyFiles,
        fun sync_worker/2, [FromDataDir, ToDataDir],
        fun sync_handler/2, [FromDataDir, ToDataDir]
    ).

sync_worker(FileName, [FromDir, ToDir]) ->
    FromFile = filename:join([FromDir, FileName]),
    ToFile = filename:join([ToDir, FileName]),

    NeedCopy = is_source_changed(FromFile, ToFile),
    Ret =
        case NeedCopy of
            true -> file:copy(FromFile, ToFile);
            _ -> skip
        end,
    {Ret, FileName}.


sync_handler({{ok, _}, FileName}, _Args) ->
    ?LOG_DEBUG("copy file ~s success", [FileName]);
sync_handler({skip, FileName}, _Args) ->
    rebar_api:debug("skip copy file ~s ", [FileName]);
sync_handler({Error, FileName}, _Args) ->
    rebar_api:abort("copy file ~s fail, error:~p ", [FileName, Error]).

create_custom(FirstList, DirList, ErlOpts) ->
    rebar_parallel:queue(FirstList,
        fun create_custom_worker/2, [DirList, ErlOpts],
        fun create_custom_handler/2, [DirList, ErlOpts]
    ).


create_custom_worker({Module, Keys, Body} = Term, [DirList, ErlOpts]) ->
    Ret =
        case check_rebuild(DirList, Module, Keys, ErlOpts) of
            {true, FromErl, ToErl} ->
                case filter_data_export(FromErl) of
                    {Head, Export, Rest} ->
                        NewExport = add_export(Export, Keys),
                        BodyStr = get_body(Module, Keys, Body),
                        plugin_util:write_file(ToErl, [Head, NewExport, Rest, "\n", BodyStr, "\n"]);
                    FileBin ->
                        BodyStr = get_body(Module, Keys, Body),
                        plugin_util:write_file(ToErl, [FileBin, "\n", BodyStr, "\n"])
                end;
            _ ->
                skip
        end,
    {Ret, Term}.

%% TODO 这里每次都全编译
check_rebuild(DirList, Module, Keys, ErlOpts) ->
    AppDir = proplists:get_value(app_dir, DirList),
    OutDir = proplists:get_value(out_dir, DirList),
    FromDir = proplists:get_value(from_data_dir, DirList),
    ToDir = proplists:get_value(to_data_dir, DirList),

    ErlName = plugin_util:to_list(Module) ++ ".erl",
    HrlName = plugin_util:to_list(Module) ++ ".hrl",
    BeamName = plugin_util:to_list(Module) ++ ".beam",


    ToErl = filename:join([ToDir, ErlName]),
    FromErl = filename:join([FromDir, ErlName]),
    HrlFile = filename:join([AppDir, "include", HrlName]),
    ObjFile = filename:join([OutDir, BeamName]),
    NeedRebuild =
        case file:read_file_info(ObjFile) of
            {ok, Obj} ->
                case file:read_file_info(ToErl) of
                    {ok, _To} ->
                        {ok, From} = file:read_file_info(FromErl),
                        case From#file_info.mtime > Obj#file_info.mtime of
                            true ->
                                %% 源码有变动需要重新编译
                                true;
                            _ ->
                                case file:read_file_info(HrlFile) of
                                    {ok, Hrl} when Hrl#file_info.mtime > Obj#file_info.mtime ->
                                        %% 头文件有变动  需要重新编译
                                        true;
                                    _ ->
                                        %% 配置索引函数有变化需要重新编译
                                        {export, Heads} = Keys,
                                        ExternFuns = proplists:get_keys(Heads),
                                        Funcs = proplists:get_keys(Module:module_info(exports)),
                                        OtherFuncs = Funcs -- [member, get, get_, list, module_info],
                                        lists:sort(ExternFuns) =/= lists:sort(OtherFuncs) orelse check_refrence(Module, DirList)
                                end
                        end;
                    _ ->
                        %% 没有头文件需要重新编译
                        true
                end;
            _ ->
                %% 没有beam文件需要重新编译
                true
        end,
    case NeedRebuild of
        true ->
            case compile:file(FromErl, ErlOpts ++ [binary, return]) of
                {ok, _, EBin, _Warn} ->
                    rebar_api:debug("Rebuild:~s success", [FromErl]),
                    {module, Module} = code:load_binary(Module, ObjFile, EBin),
                    {true, FromErl, ToErl};
                _ ->
                    false
            end;
        _ ->
            false
    end.


filter_data_export(File) ->
    {ok, FileBin} = file:read_file(File),
    case binary:match(FileBin, <<"-export([">>) of
        {Strat, _} ->
            <<Head:Strat/binary-unit:8, Rest/binary>> = FileBin,
            {Start2, _} = binary:match(Rest, <<"\n">>),
            <<Export:Start2/binary-unit:8, Rest2/binary>> = Rest,
            {Head, Export, Rest2};
        nomatch ->
            FileBin
    end.



get_body(Mod, {export, Heads}, {body, Fun}) ->
    lists:map(
        fun({FunName, _N}) ->
            case catch Fun(Mod, FunName) of
                Str when is_list(Str) ->
                    Str;
                Other ->
                    rebar_api:abort("create body file,Mod:~p, FunName:~p, Error:~p", [Mod, FunName, Other])
            end
        end, Heads
    ).


add_export(Export, {export, []}) ->
    Export;
add_export(Export, {export, Heads}) ->
    [First | _] = binary:split(Export, <<"]">>),
    Extrn = [[", ", plugin_util:to_list(Name), "/", plugin_util:to_list(N)] || {Name, N} <- Heads],
    [First, Extrn, "])."].


create_custom_handler({ok, {Module, _Keys, _Body}}, _Args) ->
    rebar_api:debug("create index code success, module:~p", [Module]);
create_custom_handler({no_change, {Module, _Keys, _Body}}, _Args) ->
    rebar_api:debug("module:~p no_change", [Module]);
create_custom_handler({skip, {Module, _Keys, _Body}}, _Args) ->
    rebar_api:debug("skip index code success, module:~p", [Module]);
create_custom_handler({Error, {Module, _Keys, _Body}}, _Args) ->
    rebar_api:abort("create index code fail, module:~p, error:~p", [Module, Error]).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).



check_refrence(sys_qqfrhtfhfth = Module, DirList) ->
    ModList = data_index:split_module(Module),
    lists:any(fun(ModRef) ->
        is_module_changed(ModRef, DirList)
              end, ModList);
check_refrence(_Mod, _DirList) ->
    false.


is_module_changed(ModRef, DirList) ->
    OutDir = proplists:get_value(out_dir, DirList),
    ToDir = proplists:get_value(to_data_dir, DirList),

    ErlName = plugin_util:to_list(ModRef) ++ ".erl",
    BeamName = plugin_util:to_list(ModRef) ++ ".beam",

    ToErl = filename:join([ToDir, ErlName]),
    ObjFile = filename:join([OutDir, BeamName]),
    is_source_changed(ToErl, ObjFile).


is_source_changed(FromFile, ToFile) ->
    case file:read_file_info(ToFile) of
        {ok, DesInfo} ->
            case file:read_file_info(FromFile) of
                {ok, SrcInfo} ->
                    SrcInfo#file_info.mtime > DesInfo#file_info.mtime;
                _ ->
                    true
            end;
        _ ->
            true
    end.






