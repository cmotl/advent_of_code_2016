-module(ipv7_test).
-include_lib("eunit/include/eunit.hrl").

-import(ipv7, [parse/1, abba/1, supports_tls/1]).

should_parse_line_with_one_hypernet_sequence_test() -> 
    Ip = "a[b]",
    Expected = {["a"], ["b"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_one_hypernet_sequence_and_two_non_test() -> 
    Ip = "a[b]c",
    Expected = {["a", "c"], ["b"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_two_hypernet_sequences_and_two_non_test() -> 
    Ip = "a[b]c[d]",
    Expected = {["a", "c"], ["b", "d"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_two_hypernet_sequences_and_three_non_test() -> 
    Ip = "a[b]c[d]e",
    Expected = {["a", "c", "e"], ["b", "d"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).


should_not_find_abba_in_string_less_than_4_characters_in_length_test() ->
    ?assertEqual(false, abba("")),
    ?assertEqual(false, abba("a")),
    ?assertEqual(false, abba("aa")),
    ?assertEqual(false, abba("aaa")).

should_not_find_abba_when_inner_characters_are_not_different_test() ->
    ?assertEqual(false, abba("aaaa")).

should_find_abba_in_abba_string_test() ->
    ?assertEqual(true, abba("abba")).

should_not_find_abba_in_nonabba_string_test() ->
    ?assertEqual(false, abba("abcd")).

should_find_abba_in_abba_string_with_other_characters_at_end_test() ->
    ?assertEqual(true, abba("abbaasdfl")).

should_find_abba_in_abba_string_starting_with_other_characters_test() ->
    ?assertEqual(true, abba("qweabba")).

should_not_find_abba_in_nonabba_string_with_more_than_4_characters_test() ->
    ?assertEqual(false, abba("qwertyuiop")).



should_support_tls_when_abba_in_non_hyper_net_sequences_test() ->
    HNS = ["mnop"],
    NHNS = ["abba", "qrst"],
    ?assertEqual(true, supports_tls({NHNS, HNS})).

should_not_support_tls_when_abba_in_hyper_net_sequences_test() ->
    HNS = ["bddb"],
    NHNS = ["abcd", "xyyx"],
    ?assertEqual(false, supports_tls({NHNS, HNS})).

should_not_support_tls_when_abba_not_in_non_hyper_net_sequences_test() ->
    HNS = ["qwer"],
    NHNS = ["aaaa", "tyui"],
    ?assertEqual(false, supports_tls({NHNS, HNS})).

should_support_tls_when_abba_in_larger_string_in_non_hyper_net_sequences_test() ->
    HNS = ["asdfgh"],
    NHNS = ["ioxxoj", "zxcvbn"],
    ?assertEqual(true, supports_tls({NHNS, HNS})).

