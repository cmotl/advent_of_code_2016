-module(one_time_pad).
-export([generate_hash/2, contains_triple/1, contains_quintuple/1, contains_exact_quintuple/2, is_key/2, quintuple_in_next_1000/3, generate_keys/3]).

generate_hash(Salt, Index) ->
    md5:md5_hex(Salt ++ integer_to_list(Index)).
    
contains_triple([]) -> {false};
contains_triple([A,A,A|_]) -> {true, A};
contains_triple([_|Rest]) -> contains_triple(Rest).

contains_quintuple([]) -> {false};
contains_quintuple([A,A,A,A,A|_]) -> {true, A};
contains_quintuple([_|Rest]) -> contains_quintuple(Rest).

contains_exact_quintuple([], _Character) -> false;
contains_exact_quintuple([C,C,C,C,C|_], C) -> true;
contains_exact_quintuple([_|Rest], Character) -> contains_exact_quintuple(Rest, Character).

quintuple_in_next_1000(_Salt, _Index, _Character, 1001) -> false;
quintuple_in_next_1000(Salt, Index, Character, Count) -> 
    case contains_exact_quintuple(generate_hash(Salt, Index+Count), Character) of
        true -> true;
        false -> quintuple_in_next_1000(Salt, Index, Character, Count+1)
    end.
    
quintuple_in_next_1000(Salt, Index, Character) ->
    quintuple_in_next_1000(Salt, Index, Character, 1).

is_key(Salt, Index) ->
    case contains_triple(generate_hash(Salt, Index)) of
        {true, Char} -> quintuple_in_next_1000(Salt, Index, Char);
        {false} -> false
    end.

generate_keys(Salt, Index, NumberOfKeys, Keys) -> 
    case length(Keys) == NumberOfKeys of
        true -> Keys;
        false -> 
            case is_key(Salt, Index) of
                true -> io:format("Found Key #~p at index ~p~n", [length(Keys) + 1, Index]),
                        generate_keys(Salt, Index+1, NumberOfKeys, [Index|Keys]);
                false -> generate_keys(Salt, Index+1, NumberOfKeys, Keys)
            end
    end.

generate_keys(Salt, Index, NumberOfKeys) ->
    generate_keys(Salt, Index, NumberOfKeys, []).
