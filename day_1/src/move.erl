-module(move).
-export([move/3]).

move(west, {X,Y}, Blocks) -> {X-Blocks, Y};
move(east, {X,Y}, Blocks) -> {X+Blocks, Y};

move(north, {X,Y}, Blocks) -> {X, Y+Blocks};
move(south, {X,Y}, Blocks) -> {X, Y-Blocks}.
