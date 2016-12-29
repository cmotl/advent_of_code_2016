-module(day_8_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun two_factor_authentication:parseInstruction/1, SplitLines),
    Board = two_factor_authentication:create_screen(6,50),
    FinalBoard = dict:to_list(lists:foldl(fun two_factor_authentication:execute_instruction/2, Board, Instructions)),
    OnVsOff = lists:map(fun({_,State}) -> case State of on -> 1; off -> 0 end end, FinalBoard),
    ?assertEqual(128, lists:foldl(fun(X, Sum) -> Sum + X end, 0, OnVsOff)).

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun two_factor_authentication:parseInstruction/1, SplitLines),
    Board = two_factor_authentication:create_screen(6,50),
    FinalBoard = lists:foldl(fun two_factor_authentication:execute_instruction/2, Board, Instructions),
    Rows = lists:map(fun(Row) -> lists:map(fun(X) -> case X of on -> $#; off -> $. end end,  two_factor_authentication:row_of_pixels_from_board(Row, FinalBoard))  end, [X || X<-lists:seq(0,5)]),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Rows),
    ?assert(true).
