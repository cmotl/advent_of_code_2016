-module(intermediate_locations).
-export([intermediate_locations/2]).

intermediate_locations({X,Y}, {X,Y}) -> [];
intermediate_locations({X1,Y}, {X2,Y}) when X2 > X1 -> [{X1+1,Y}|intermediate_locations({X1+1, Y},{X2,Y})] ;
intermediate_locations({X1,Y}, {X2,Y}) when X2 < X1 -> [{X1-1,Y}|intermediate_locations({X1-1, Y},{X2,Y})] ;
intermediate_locations({X,Y1}, {X,Y2}) when Y2 > Y1 -> [{X,Y1+1}|intermediate_locations({X,Y1+1} ,{X,Y2})];
intermediate_locations({X,Y1}, {X,Y2}) when Y2 < Y1 -> [{X,Y1-1}|intermediate_locations({X,Y1-1} ,{X,Y2})].
