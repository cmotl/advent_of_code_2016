-module(hex_keypad).
-export([map/2]).

map("1", up) -> "1";
map("1", down) -> "3";
map("1", left) -> "1";
map("1", right) -> "1";

map("2", up) -> "2";
map("2", down) -> "6";
map("2", left) -> "2";
map("2", right) -> "3";

map("3", up) -> "1";
map("3", down) -> "7";
map("3", left) -> "2";
map("3", right) -> "4";

map("4", up) -> "4";
map("4", down) -> "8";
map("4", left) -> "3";
map("4", right) -> "4";

map("5", up) -> "5";
map("5", down) -> "5";
map("5", left) -> "5";
map("5", right) -> "6";

map("6", up) -> "2";
map("6", down) -> "A";
map("6", left) -> "5";
map("6", right) -> "7";

map("7", up) -> "3";
map("7", down) -> "B";
map("7", left) -> "6";
map("7", right) -> "8";

map("8", up) -> "4";
map("8", down) -> "C";
map("8", left) -> "7";
map("8", right) -> "9";

map("9", up) -> "9";
map("9", down) -> "9";
map("9", left) -> "8";
map("9", right) -> "9";

map("A", up) -> "6";
map("A", down) -> "A";
map("A", left) -> "A";
map("A", right) -> "B";

map("B", up) -> "7";
map("B", down) -> "D";
map("B", left) -> "A";
map("B", right) -> "C";

map("C", up) -> "8";
map("C", down) -> "C";
map("C", left) -> "B";
map("C", right) -> "C";

map("D", up) -> "B";
map("D", down) -> "D";
map("D", left) -> "D";
map("D", right) -> "D".
