-module(ipv7_test).
-include_lib("eunit/include/eunit.hrl").

-import(ipv7, [parse/1, abba/1, supports_tls/1, aba/1, extract_abas/1, invert_aba/1, supports_ssl/1]).

should_parse_line_with_one_hypernet_sequence_test() -> 
    Ip = "a[b]",
    Expected = {["a"], ["b"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_one_hypernet_sequence_and_two_supernet_sequences_test() -> 
    Ip = "a[b]c",
    Expected = {["a", "c"], ["b"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_two_hypernet_sequences_and_two_supernet_sequences_test() -> 
    Ip = "a[b]c[d]",
    Expected = {["a", "c"], ["b", "d"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).

should_parse_line_with_two_hypernet_sequences_and_three_supernet_sequences_test() -> 
    Ip = "a[b]c[d]e",
    Expected = {["a", "c", "e"], ["b", "d"]},
    Actual = parse(Ip),
    ?assertEqual(Expected, Actual).


should_invert_aba_test() ->
    ?assertEqual("bab", invert_aba("aba")).


should_not_find_aba_in_string_less_than_3_characters_in_length_test() ->
    ?assertEqual(false, aba("")),
    ?assertEqual(false, aba("a")),
    ?assertEqual(false, aba("aa")).

should_not_find_aba_when_inner_characters_are_not_different_test() ->
    ?assertEqual(false, aba("aaa")).

should_find_aba_aba_string_test() ->
    ?assertEqual(true, aba("aba")).


should_find_one_aba_string_in_aba_string_test() ->
    ?assertEqual(["aba"], extract_abas("aba")).

should_find_no_aba_string_in_non_aba_string_test() ->
    ?assertEqual([], extract_abas("aaa")).

should_find_one_aba_string_in_aba_string_with_trailing_characters_test() ->
    ?assertEqual(["aba"], extract_abas("abaccc")).

should_find_one_aba_string_in_aba_string_with_leading_characters_test() ->
    ?assertEqual(["aba"], extract_abas("cccaba")).

should_find_two_aba_strings_test() ->
    ?assertEqual(["xyx", "aba"], extract_abas("xyxaba")).


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



should_support_tls_when_abba_in_supernet_sequences_test() ->
    HNS = ["mnop"],
    SNS = ["abba", "qrst"],
    ?assertEqual(true, supports_tls({SNS, HNS})).

should_not_support_tls_when_abba_in_hyper_net_sequences_test() ->
    HNS = ["bddb"],
    SNS = ["abcd", "xyyx"],
    ?assertEqual(false, supports_tls({SNS, HNS})).

should_not_support_tls_when_abba_not_in_supernet_sequences_test() ->
    HNS = ["qwer"],
    SNS = ["aaaa", "tyui"],
    ?assertEqual(false, supports_tls({SNS, HNS})).

should_support_tls_when_abba_in_larger_string_in_supernet_sequences_test() ->
    HNS = ["asdfgh"],
    SNS = ["ioxxoj", "zxcvbn"],
    ?assertEqual(true, supports_tls({SNS, HNS})).

should_support_ssl_when_aba_in_supernet_has_coresponding_bab_in_hypernet_test() ->
    HNS = ["bab"],
    SNS = ["aba","xyz"],
    ?assertEqual(true, supports_ssl({SNS, HNS})).

should_support_ssl_when_two_abas_overlapn_in_supernet_and_has_corresponding_bab_in_hypernet_test() ->
    HNS = ["bzb"],
    SNS = ["zazbz","cdb"],
    ?assertEqual(true, supports_ssl({SNS, HNS})).


