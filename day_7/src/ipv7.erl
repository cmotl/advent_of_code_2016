-module(ipv7).
-export([parse/1, abba/1, supports_tls/1]).

parse(Ip) -> 
    Components = re:split(Ip,"(\\[|\\])",[{return,list}, group]),
{UnTrimmedHypernetSequences, UnTrimmedNonHypernetSequences} = lists:partition(fun(X) -> lists:member("]", X) end, Components),
    HypernetSequences = re:split(lists:flatten(UnTrimmedHypernetSequences), "]",[{return,list}, trim]),
    NonHypernetSequences = re:split(lists:flatten(UnTrimmedNonHypernetSequences), "[[]", [{return, list}, trim]),
    {NonHypernetSequences, HypernetSequences}.

abba([]) -> false;
abba([_|[]]) -> false;
abba([_,_|[]]) -> false;
abba([_,_,_|[]]) -> false;
abba([A,B,B,A|_]) when A =/= B -> true; 
abba([_,_,_,_|[]]) -> false;
abba([_,A,B,C|Rest]) -> abba([A,B,C|Rest]).

supports_tls({NHNS, HNS}) -> 
    TlsInNhns = lists:member(true, lists:map(fun(X) -> abba(X) end, NHNS)),
    TlsInHns = lists:member(true, lists:map(fun(X) -> abba(X) end, HNS)),
    TlsInNhns and not TlsInHns.
