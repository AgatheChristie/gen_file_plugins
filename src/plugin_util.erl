-module(plugin_util).


-export([write_file/2, ensure_dir/1, to_atom/1, to_list/1]).



to_atom(BaseName) when is_atom(BaseName) ->
    BaseName;
to_atom(BaseName) when is_binary(BaseName) ->
    case catch erlang:binary_to_existing_atom(BaseName, utf8) of
        {'EXIT', _} ->
            erlang:binary_to_atom(BaseName, utf8);
        Atom ->
            Atom
    end;
to_atom(BaseName) when is_list(BaseName) ->
    case catch erlang:list_to_existing_atom(BaseName) of
        {'EXIT', _} ->
            list_to_atom(BaseName);
        Atom ->
            Atom
    end.


to_list(N) when is_list(N) -> N;
to_list(N) when is_binary(N) -> erlang:binary_to_list(N);
to_list(N) when is_atom(N) -> erlang:atom_to_list(N);
to_list(N) when is_integer(N) -> erlang:integer_to_list(N);
to_list(N) when is_float(N) -> erlang:float_to_list(N, [{decimals, 4}, compact]).




ensure_dir(Dir) ->
    Path = filename:join(Dir, "dummy.beam"),
    filelib:ensure_dir(Path).


md5(Data) ->
    md5(Data, lower).


md5(Data, CharCase) ->
    <<N:128>> = erlang:md5(Data),
    Format =
        case CharCase of
            upper -> "~32.16.0B";
            _ -> "~32.16.0b"
        end,
    lists:flatten(io_lib:format(Format, [N])).

write_file(FileName, FileContent) ->
    NeedWriteToFile =
        case file:read_file(FileName) of
            {ok, OldBin} ->
                OldMd5 = md5(OldBin),
                NewMd5 = md5(iolist_to_binary(FileContent)),
                OldMd5 =/= NewMd5;
            _ ->
                true
        end,


    case NeedWriteToFile of
        true ->
            case file:write_file(FileName, FileContent) of
                ok ->
                    rebar_api:info("write ~s success", [FileName]),
                    ok;
                Err ->
                    rebar_api:abort("write ~s err, ~p", [FileName, Err])
            end;
        _ ->
            rebar_api:debug("~s not change", [FileName]),
            no_change
    end.






















