-module(keypad).
-export([map/2]).

map(1, up) -> 1;
map(1, down) -> 4;
map(1, left) -> 1;
map(1, right) -> 2;

map(2, up) -> 2;
map(2, down) -> 5;
map(2, left) -> 1;
map(2, right) -> 3;

map(3, up) -> 3;
map(3, down) -> 6;
map(3, left) -> 2;
map(3, right) -> 3;

map(4, up) -> 1;
map(4, down) -> 7;
map(4, left) -> 4;
map(4, right) -> 5;

map(5, up) -> 2;
map(5, down) -> 8;
map(5, left) -> 4;
map(5, right) -> 6;

map(6, up) -> 3;
map(6, down) -> 9;
map(6, left) -> 5;
map(6, right) -> 6;

map(7, up) -> 4;
map(7, down) -> 7;
map(7, left) -> 7;
map(7, right) -> 8;

map(8, up) -> 5;
map(8, down) -> 8;
map(8, left) -> 7;
map(8, right) -> 9;

map(9, up) -> 6;
map(9, down) -> 9;
map(9, left) -> 8;
map(9, right) -> 9.
