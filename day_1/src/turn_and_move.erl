-module(turn_and_move).
-export([turn_and_move/2]).

turn_and_move({TurnDirection,Blocks}, {CurrentDirecton,{X,Y}}) -> 
    NewHeading = turn:turn(CurrentDirecton, TurnDirection),
    NewCoordinates = move:move(NewHeading,{X,Y}, Blocks), 
    {NewHeading, NewCoordinates}.

