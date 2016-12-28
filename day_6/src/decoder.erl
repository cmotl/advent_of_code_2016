-module(decoder).
-export([max_character/1, most_frequent_character/1, min_character/1, least_frequent_character/1]).

s_max({K1,V1}, {K2,V2}) when V1 == V2 -> K1 < K2;
s_max({_,V1}, {_,V2}) -> V1 >= V2.

s_min({K1,V1}, {K2,V2}) when V1 == V2 -> K1 < K2;
s_min({_,V1}, {_,V2}) -> V1 < V2.

max_character(Aggregate) ->
    case dict:is_empty(Aggregate) of
        true -> nil;
        false -> A = dict:to_list(Aggregate),
                AlphaCount = lists:sort(fun s_max/2, A),
                Alpha = lists:map(fun({K,_}) -> K end, AlphaCount),
                lists:nth(1, Alpha)
    end.

min_character(Aggregate) ->
    case dict:is_empty(Aggregate) of
        true -> nil;
        false -> A = dict:to_list(Aggregate),
                AlphaCount = lists:sort(fun s_min/2, A),
                Alpha = lists:map(fun({K,_}) -> K end, AlphaCount),
                lists:nth(1, Alpha)
    end.

most_frequent_character(Characters) -> 
    Aggregate = lists:foldl(fun update_aggregate/2, dict:new(), Characters),
    max_character(Aggregate).

least_frequent_character(Characters) -> 
    Aggregate = lists:foldl(fun update_aggregate/2, dict:new(), Characters),
    min_character(Aggregate).

%most_frequent_character(Characters) -> 
%    most_frequent_character(Characters, dict:new()).

%most_frequent_character([], Aggregate) -> max_character(Aggregate);
%most_frequent_character([Character|Rest], Aggregate) -> 
%    NewAggregate = update_aggregate(Character, Aggregate),
%    most_frequent_character(Rest, NewAggregate).

update_aggregate(Character, Aggregate) ->
    dict:update(Character, fun(V) -> V + 1 end, 1, Aggregate).
