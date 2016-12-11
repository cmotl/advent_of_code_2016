-module(day_2_test).
-include_lib("eunit/include/eunit.hrl").

integration1_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawInstructions = binary:bin_to_list(Binary),
    SplitInstructions = string:tokens(RawInstructions, "\n"),
    InstructionsSet = lists:map(fun(Instructions) -> instruction:parse(Instructions) end, SplitInstructions),
    ?assertEqual("19636", digit:decode_instructions(InstructionsSet)).
