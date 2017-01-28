-module(day_14_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    Salt = "cuanljph",
    ?assertEqual(8, length(Salt)).

part2_integration_test() -> 
    Salt = "cuanljph",
    ?assertEqual(8, length(Salt)).



% part 1
% one_time_pad:generate_keys("cuanljph", 0, 64).


% part 2
% hash_server:start("cuanljph").
% secure_one_time_pad:generate_keys("", 0, 64). 
