-module(couch_pb_codec).

-export([encode/2]).
-export([decode/2]).
-export([get_command_name/1]).
-export([get_command_num/1]).

-include("include/couch_pb.hrl").
-include("include/couch_pb_codec.hrl").

-spec encode(req | resp, #cpbrequest{} | #cpbresponse{}) -> binary().
encode(req, Req) ->
    iolist_to_binary(couch_pb:encode_cpbrequest(Req));
encode(resp, Resp) ->
    iolist_to_binary(couch_pb:encode_cpbresponse(Resp)).

-spec decode(req | resp, binary()) -> #cpbrequest{} | #cpbresponse{}.
decode(req, Req) ->
    couch_pb:decode_cpbrequest(Req);
decode(resp, Resp) ->
    couch_pb:decode_cpbresponse(Resp).

-spec get_command_name(non_neg_integer()) -> atom() | {error, not_found}.
get_command_name(N) ->
    case lists:keyfind(N, 1, ?COMMANDS) of
        {N, C} ->  C;
        _ -> {error, not_found}
    end.

-spec get_command_num(atom()) -> non_neg_integer() | {error, not_found}.
get_command_num(C) ->
    case lists:keyfind(C, 2, ?COMMANDS) of
        {N, C} -> N;
        _ -> {error, not_found}
    end.
