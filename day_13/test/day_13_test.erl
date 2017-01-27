-module(day_13_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    FavoriteNumber = 1364,
    {ok, Schematic} = schematic:start(FavoriteNumber),
    StartLocation = {1,1},
    Destination = {31,39},
    {ActiveSearchSpace, _DeadEndSearchSpace} = path_finder:paths_to_destination(StartLocation, Destination, Schematic),
    Distances = lists:map(fun(X) -> io:format("~p~n", [length(X)]), length(X) end, ActiveSearchSpace),
    ?assertEqual(86, hd(Distances)-1).

part2_integration_test() -> 
    FavoriteNumber = 1364,
    {ok, Schematic} = schematic:start(FavoriteNumber),
    StartLocation = {1,1},
    NumberOfSteps = 50,
    UniqueLocations = path_finder:unique_locations_in_steps(StartLocation, NumberOfSteps, Schematic),
    ?assertEqual(127, length(UniqueLocations)).


%    Rows = [ [{X,Y} || X <- lists:seq(0,9) ] || Y <- lists:seq(0,6) ],
%    Rows1 = lists:map(fun(Row) -> lists:map(fun({X,Y}) -> case schematic:element_at(S,X,Y) of wall -> $#; open_space -> $. end end, Row) end, Rows),
%    io:format("~n",[]),
%    lists:foreach(fun(Row) -> io:format("~p~n", [Row]) end, Rows1),
