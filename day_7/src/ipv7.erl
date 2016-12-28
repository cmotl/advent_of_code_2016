-module(ipv7).
-export([parse/1, abba/1, supports_tls/1, aba/1, extract_abas/1, invert_aba/1, supports_ssl/1]).

parse(Ip) -> 
    Components = re:split(Ip,"(\\[|\\])",[{return,list}, group]),
{UnTrimmedHypernetSequences, UnTrimmedSupernetSequences} = lists:partition(fun(X) -> lists:member("]", X) end, Components),
    HypernetSequences = re:split(lists:flatten(UnTrimmedHypernetSequences), "]",[{return,list}, trim]),
    SupernetSequences = re:split(lists:flatten(UnTrimmedSupernetSequences), "[[]", [{return, list}, trim]),
    {SupernetSequences, HypernetSequences}.

abba([]) -> false;
abba([_|[]]) -> false;
abba([_,_|[]]) -> false;
abba([_,_,_|[]]) -> false;
abba([A,B,B,A|_]) when A =/= B -> true; 
abba([_,_,_,_|[]]) -> false;
abba([_,A,B,C|Rest]) -> abba([A,B,C|Rest]).

aba([]) -> false;
aba([_|[]]) -> false;
aba([_,_|[]]) -> false;
aba([A,B,A|[]]) when A =/= B -> true;
aba([_,_,_|[]]) -> false.


extract_abas(Input) -> extract_abas(Input, []).

extract_abas([_,_|[]], Abas) -> Abas;
extract_abas([A,B,C|Rest], Abas) ->
    case aba([A,B,C|[]]) of
        true -> extract_abas([B,C|Rest], Abas ++ [[A,B,C]]);
        _ -> extract_abas([B,C|Rest],Abas)
    end.

invert_aba([A,B,A]) -> [B,A,B].


supports_tls({SNS, HNS}) -> 
    TlsInSns = lists:member(true, lists:map(fun(X) -> abba(X) end, SNS)),
    TlsInHns = lists:member(true, lists:map(fun(X) -> abba(X) end, HNS)),
    TlsInSns and not TlsInHns.

supports_ssl({SNS, HNS}) -> 
    Abas = lists:foldl(fun(X, Xs) -> ipv7:extract_abas(X) ++ Xs end, [], SNS),
    Babs = lists:map(fun ipv7:invert_aba/1, lists:foldl(fun(X, Xs) -> ipv7:extract_abas(X) ++ Xs end , [], HNS)),
    Matches = sets:intersection(sets:from_list(Abas), sets:from_list(Babs)),
    sets:size(Matches) >= 1.

