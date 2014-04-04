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
-export([encode_kv/1]).
-export([decode_kv/1]).
-export([encode_doc/1]).
-export([decode_doc/1]).
-export([get_command_name/1]).
-export([get_command_id/1]).

-include("include/couch_pb.hrl").

-type key_value() :: {binary(), binary()}.
-type document() :: [key_value()].

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

-spec encode_kv(key_value()) -> #cpbkv{}.
encode_kv({K, V}) ->
    #cpbkv{key = K, value = V}.

-spec decode_kv(#cpbkv{}) -> key_value().
decode_kv(#cpbkv{key = K, value = V}) ->
    {K, V}.

-spec encode_doc(document()) -> #cpbdoc{}.
encode_doc(Doc) ->
    #cpbdoc{kv = [#cpbkv{key = K, value = V} || {K, V} <- Doc]}.

-spec decode_doc(#cpbdoc{}) -> document().
decode_doc(#cpbdoc{kv = KVs}) ->
    [decode_kv(KV) || KV <- KVs].

-spec get_command_name(non_neg_integer()) -> atom() | {error, not_found}.
get_command_name(N) ->
    case lists:keyfind(N, 1, commands()) of
        {N, C} ->  C;
        _ -> {error, not_found}
    end.

-spec get_command_id(atom()) -> non_neg_integer() | {error, not_found}.
get_command_id(C) ->
    case lists:keyfind(C, 2, commands()) of
        {N, C} -> N;
        _ -> {error, not_found}
    end.


%% -----------------------------------------------------------------------------
%% tests
%% -----------------------------------------------------------------------------
-ifdef(TEST).

commands_test() ->
    commands() =/= [].

get_command_name_test() ->
    true = is_atom(get_command_name(2)),
    true = is_atom(get_command_name(21)),
    {error, not_found} = get_command_name(-86),
    {error, not_found} = get_command_name(wat_da___).

get_command_id_test() ->
    true = is_integer(get_command_id(all_dbs)),
    true = is_integer(get_command_id(stats)),
    {error, not_found} = get_command_id(wut),
    {error, not_found} = get_command_id(-69).

-endif.
