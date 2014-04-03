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


%% -----------------------------------------------------------------------------
%% tests
%% -----------------------------------------------------------------------------
-ifdef(TEST).

encoded_req() ->
    <<8,3,18,13,99,111,117,99,104,45,112,98,45,116,101,115,116>>.

decoded_req() ->
    #cpbrequest{command = 3, db_name = "couch-pb-test"}.

encoded_resp() ->
    <<8,1,18,11,115,111,109,101,32,114,101,115,117,108,116>>.

decoded_resp() ->
    #cpbresponse{resp_code = 1, result="some result"}.

commands_test() ->
    commands() =/= [].

encode_req_test() ->
    true = (encoded_req() =:= encode_req(decoded_req())).

decode_req_test() ->
    true = (decoded_req() =:= decode_req(encoded_req())).

encode_resp_test() ->
    true = (encoded_resp() =:= encode_resp(decoded_resp())).

decode_resp_test() ->
    true = (decoded_resp() =:= decode_resp(encoded_resp())).

get_command_name_test() ->
    all_dbs = get_command_name(2).

get_command_num_test() ->
    2 = get_command_num(all_dbs).

-endif.
