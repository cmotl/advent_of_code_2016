-module(day_7_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Ips = lists:map(fun ipv7:parse/1, SplitLines),
    TlsIps = lists:filter(fun ipv7:supports_tls/1, Ips),
    ?assertEqual(110, length(TlsIps)).
