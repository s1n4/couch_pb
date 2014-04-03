-module(couch_pb_codec).

%% -----------------------------------------------------------------------------
%% export and append commands/0
%% -----------------------------------------------------------------------------
-compile({parse_transform, couch_pb_transform}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-export([encode_req/1]).
-export([encode_resp/1]).
-export([decode_req/1]).
-export([decode_resp/1]).
-export([get_command_name/1]).
-export([get_command_num/1]).

-include("include/couch_pb.hrl").

-spec encode_req(#cpbrequest{}) -> binary().
encode_req(Req) ->
    iolist_to_binary(couch_pb:encode_cpbrequest(Req)).

-spec encode_resp(#cpbresponse{}) -> binary().
encode_resp(Resp) ->
    iolist_to_binary(couch_pb:encode_cpbresponse(Resp)).

-spec decode_req(binary()) -> #cpbrequest{}.
decode_req(Req) ->
    couch_pb:decode_cpbrequest(Req).

-spec decode_resp(binary()) -> #cpbresponse{}.
decode_resp(Resp) ->
    couch_pb:decode_cpbresponse(Resp).

-spec get_command_name(non_neg_integer()) -> atom() | {error, not_found}.
get_command_name(N) ->
    case lists:keyfind(N, 1, commands()) of
        {N, C} ->  C;
        _ -> {error, not_found}
    end.

-spec get_command_num(atom()) -> non_neg_integer() | {error, not_found}.
get_command_num(C) ->
    case lists:keyfind(C, 2, commands()) of
        {N, C} -> N;
        _ -> {error, not_found}
    end.
