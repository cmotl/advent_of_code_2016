-module(kinetic_sculpture_test).
-include_lib("eunit/include/eunit.hrl").

-import(kinetic_sculpture, [disc_is_open/2, falls_through/2, time_to_release_capsule_for_fall_through/1]).

should_be_open_at_position_zero_test() ->
    Positions = 2,
    StartingPosition = 0,
    DiscPosition = 0,
    Time = 0,
    Disc = {Positions, StartingPosition, DiscPosition},
    ?assertEqual(true, disc_is_open(Disc, Time)).

should_be_closed_at_positions_other_than_zero_test() ->
    Positions = 2,
    StartingPosition = 1,
    DiscPosition = 0,
    Time = 0,
    Disc = {Positions, StartingPosition, DiscPosition},
    ?assertEqual(false, disc_is_open(Disc, Time)).

should_be_closed_at_time_after_starting_open_test() ->
    Positions = 2,
    StartingPosition = 0,
    DiscPosition = 0,
    Time = 1,
    Disc = {Positions, StartingPosition, DiscPosition},
    ?assertEqual(false, disc_is_open(Disc, Time)).

should_be_open_at_time_after_starting_closed_test() ->
    Positions = 2,
    StartingPosition = 1,
    DiscPosition = 0,
    Time = 1,
    Disc = {Positions, StartingPosition, DiscPosition},
    ?assertEqual(true, disc_is_open(Disc, Time)).

should_also_account_for_disc_position_test() ->
    Positions = 2,
    StartingPosition = 1,
    DiscPosition = 1,
    Time = 1,
    Disc = {Positions, StartingPosition, DiscPosition},
    ?assertEqual(false, disc_is_open(Disc, Time)).

should_fall_through_single_disc_test() ->
    Discs = [{2, 1, 1}],
    Time = 0,
    ?assertEqual(true, falls_through(Discs, Time)).

should_fall_through_multiple_disc_test() ->
    Discs = [{5, 4, 1}, {2, 1, 2}],
    Time = 5,
    ?assertEqual(true, falls_through(Discs, Time)).

should_find_the_first_time_to_release_capsule_for_complete_fall_through_test() ->
    Discs = [{5, 4, 1}, {2, 1, 2}],
    ExpectedTime = 5,
    ?assertEqual(ExpectedTime, time_to_release_capsule_for_fall_through(Discs)).
