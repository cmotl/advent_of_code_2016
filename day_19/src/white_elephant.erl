-module(white_elephant).
-export([steal_gifts/2, exchange_gifts/1, exchange_gifts_across_fast/1, exchange_gifts_across_slow/1]).

steal_gifts({LeftElfId, LeftElfGiftCount},{RightElfId, RightElfGiftCount}) ->
    {{LeftElfId, LeftElfGiftCount+RightElfGiftCount}, {RightElfId, 0}}.

exchange_gifts([LeftElf], [], _GiftlessElves) -> LeftElf;
exchange_gifts([LeftElf], GiftedElves, _GiftlessElves) -> 
    exchange_gifts([LeftElf|lists:reverse(GiftedElves)], [], GiftedElves);
exchange_gifts([], GiftedElves, GiftlessElves) -> 
    exchange_gifts(lists:reverse(GiftedElves), [], GiftlessElves);
exchange_gifts([LeftElf,RightElf|Rest], GiftedElves, GiftlessElves) ->
    {NewLeftElf, NewRightElf} = steal_gifts(LeftElf, RightElf),
    exchange_gifts(Rest, [NewLeftElf|GiftedElves], [NewRightElf|GiftlessElves]).

exchange_gifts_slow([LeftElf], _GiftlessElves) -> LeftElf;
exchange_gifts_slow([LeftElf,RightElf|Rest], GiftlessElves) ->
    {NewLeftElf, NewRightElf} = steal_gifts(LeftElf, RightElf),
    exchange_gifts_slow(lists:append([Rest, [NewLeftElf]]), [NewRightElf|GiftlessElves]).


exchange_gifts_across_slow_([LeftElf]) -> LeftElf;
exchange_gifts_across_slow_(Elves) ->
    FirstHalf = lists:sublist(Elves, 1, length(Elves) div 2),
    SecondHalf = lists:sublist(Elves, (length(Elves) div 2) + 1, length(Elves)),
    [Thief|RestOfFirst] = FirstHalf,
    [_Victim|RestOfSecond] = SecondHalf,
    exchange_gifts_across_slow_(lists:append([RestOfFirst, RestOfSecond, [Thief]])).

exchange_gifts(Elves) ->
    exchange_gifts(Elves, [], []).

exchange_gifts_across_slow(Elves) ->
    exchange_gifts_across_slow_(Elves).

exchange_gifts_across_fast(NumberOfElves) ->
    ElvesLog = math:log10(NumberOfElves),
    LogThree = math:log10(3),
    Base = trunc(ElvesLog / LogThree),
    Midpoint = trunc(math:pow(3, Base+1) - math:pow(3,Base)),
    Diff = Midpoint - NumberOfElves,
    HalfOfUpperMidPoint = trunc(Midpoint / 2),
    HalfOfUpperMidPoint - Diff.
