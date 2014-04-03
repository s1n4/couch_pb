-module(couch_pb_transform).

-export([parse_transform/2]).

%% -----------------------------------------------------------------------------
%% parse transform
%% -----------------------------------------------------------------------------
parse_transform(AST, _Options) ->
    walk_ast(AST, []).

walk_ast([], Acc) ->
    lists:reverse(Acc);
%% -----------------------------------------------------------------------------
%% export commands/0
%% -----------------------------------------------------------------------------
walk_ast([{attribute, L, module, _}=Form|Rest], Acc) ->
    Acc1 = [Form|Acc],
    walk_ast(Rest, [{attribute, L, export, [{commands, 0}]}|Acc1]);
%% -----------------------------------------------------------------------------
%% append commands/0 -> [{non_neg_integer(), atom()}]
%% -----------------------------------------------------------------------------
walk_ast([{eof, L}=Form|Rest], Acc) ->
    Commands = parse_csv_file(filename:join("src", "couch_pb_commands.csv")),
    Acc1 = [{function, L, commands, 0,
             [
              {clause, L, [], [],
               [erl_parse:abstract(Commands, [{line, L}])]}
             ]}|Acc],
    walk_ast(Rest, [Form|Acc1]);
walk_ast([Form|Rest], Acc) ->
    walk_ast(Rest, [Form|Acc]).

%% -----------------------------------------------------------------------------
%% parse CSV
%% -----------------------------------------------------------------------------
parse_csv_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Content1 = binary_to_list(Content),
    parse_csv(Content1).

parse_csv(String) ->
    LineSepTokens = string:tokens(String, "\n"),
    parse_csv_tokens(LineSepTokens, []).

parse_csv_tokens([], Acc) ->
    Acc;
parse_csv_tokens([Token|Rest], Acc) ->
    [Num, Name] = string:tokens(Token, ","),
    parse_csv_tokens(Rest, Acc ++ [{list_to_integer(Num), list_to_atom(Name)}]).
