-module(couch_pb_codec).
-compile(export_all).

-include("include/couch_pb_codec.hrl").

-spec command(non_neg_integer() | atom()) -> atom() | non_neg_integer() |
                                             {error, not_found}.
command(N) when is_integer(N) ->
    case lists:keyfind(N, 1, ?COMMANDS) of
        {N, C, _} -> C;
        _ -> {error, not_found}
    end;
command(C) when is_atom(C) ->
    case lists:keyfind(C, 2, ?COMMANDS) of
        {N, C, _} -> N;
        _ -> {error, not_found}
    end.
