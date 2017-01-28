-module(kinetic_sculpture).
-export([disc_is_open/2, falls_through/2, time_to_release_capsule_for_fall_through/1]).

disc_is_open({Positions, StartingPosition, DiscPosition}, Time) ->
    (StartingPosition + Time + DiscPosition) rem Positions == 0.

falls_through(Discs, Time) ->
    lists:all(fun(Disc) -> disc_is_open(Disc, Time) end, Discs).

time_to_release_capsule_for_fall_through(Discs)->
    time_to_release_capsule_for_fall_through(Discs, 0).

time_to_release_capsule_for_fall_through(Discs, Time) ->
    case falls_through(Discs, Time) of
        true -> Time;
        false -> time_to_release_capsule_for_fall_through(Discs, Time + 1)
    end.

