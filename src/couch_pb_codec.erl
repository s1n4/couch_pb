-module(couch_pb_codec).

-export([get_command_info/1]).
-export([get_command_mfa/2]).

-include("include/couch_pb_codec.hrl").

-type command_info() :: {non_neg_integer(), atom(), {module(), atom()}}.

-spec get_command_info(non_neg_integer() | atom()) -> command_info() |
                                                      {error, not_found}.
get_command_info(N) when is_integer(N) ->
    case lists:keyfind(N, 1, ?COMMANDS) of
        X = {N, _, _} ->  X;
        _ -> {error, not_found}
    end;
get_command_info(C) when is_atom(C) ->
    case lists:keyfind(C, 2, ?COMMANDS) of
        X = {_, C, _} -> X;
        _ -> {error, not_found}
    end.

-spec get_command_mfa(command_info(), [any()]) -> mfa().
get_command_mfa({_, _, {M, F}}, A) ->
    {M, F, A}.
