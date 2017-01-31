-module(white_elephant_test).
-include_lib("eunit/include/eunit.hrl").

-import(white_elephant, [steal_gifts/2, exchange_gifts/1]).

should_steal_gifts_between_two_elves_test() -> 
    LeftElf = {1, 1},
    RightElf = {2, 2},
    ExpectedElves = {{1,3}, {2,0}},
    ?assertEqual(ExpectedElves, steal_gifts(LeftElf, RightElf)).

should_exchange_gifts_between_multiple_elves_test() ->
    Elves = [{1,1},{2,1},{3,1},{4,1},{5,1}],
    FinalElf = {3,5},
    ?assertEqual(FinalElf, exchange_gifts(Elves)).
    
