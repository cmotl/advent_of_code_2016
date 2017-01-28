-module(day_16_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    InitialData = "10010000000110000",
    MaxLength = 272,
    DragonCurveData = random_data:dragon_curve(InitialData, MaxLength),
    Checksum = random_data:dragon_curve_checksum(DragonCurveData),
    ?assertEqual("10010110010011110", Checksum).

part2_integration_test_() -> 
    InitialData = "10010000000110000",
    MaxLength = 35651584,
    DragonCurveData = random_data:dragon_curve(InitialData, MaxLength),
    Checksum = random_data:dragon_curve_checksum(DragonCurveData),
    {timeout, 60, ?_assertEqual("01101011101100011", Checksum)}.
