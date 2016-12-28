-module(day_6_test).
-include_lib("eunit/include/eunit.hrl").

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

should_transpose_list_of_lists_test() ->
    ExpectedList = [[1,2,3],[1,2,3],[1,2,3]],
    ActualList = transpose([[1,1,1],[2,2,2],[3,3,3]]),
    ?assertEqual(ExpectedList, ActualList).

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawCodes = binary:bin_to_list(Binary),
    SplitCodes = string:tokens(RawCodes, "\n"),
    TransposedCodes = transpose(SplitCodes),
    DecodedMessage = lists:map(fun decoder:most_frequent_character/1, TransposedCodes), 
    ?assertEqual("tzstqsua", DecodedMessage).

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawCodes = binary:bin_to_list(Binary),
    SplitCodes = string:tokens(RawCodes, "\n"),
    TransposedCodes = transpose(SplitCodes),
    DecodedMessage = lists:map(fun decoder:least_frequent_character/1, TransposedCodes), 
    ?assertEqual("myregdnr", DecodedMessage).
