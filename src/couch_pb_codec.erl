-module(couch_pb_codec).
-compile(export_all).

-include("include/couch_pb_codec.hrl").

-spec command(non_neg_integer() | atom()) -> atom() | non_neg_integer() |
                                             {error, not_found}.
command(N) when is_integer(N) ->
    case lists:keyfind(N, 2, ?COMMANDS) of
        {C, N} -> C;
        _ -> {error, not_found}
    end;
command(C) when is_atom(C) ->
    case lists:keyfind(C, 1, ?COMMANDS) of
        {C, N} -> N;
        _ -> {error, not_found}
    end.
