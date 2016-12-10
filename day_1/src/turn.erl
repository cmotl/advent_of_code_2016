-module(turn).
-export([turn/2]).

turn(north, left) -> west;
turn(north, right) -> east;

turn(west, left) -> south;
turn(west, right) -> north;

turn(south, left) -> east;
turn(south, right) -> west;

turn(east, left) -> north;
turn(east, right) -> south.

