-module(distance).
-export([distance/1]).

distance({X,Y}) -> abs(X) + abs(Y).
