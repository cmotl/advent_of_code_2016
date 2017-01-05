-module(decompressor).
-export([decompress/1]).

contains_marker([_,_,_,_]) -> true;
contains_marker([_]) -> false.

expand([Prefix, SizeString, CountString, Suffix]) -> 
    Size = list_to_integer(SizeString),
    Count = list_to_integer(CountString),
    Repeat = string:substr(Suffix, 1, Size),
    Remainder = string:substr(Suffix, 1+Size),
    Decompressed = lists:flatten(lists:duplicate(Count, Repeat)),
    {Prefix, Decompressed, Remainder}.

decompress(Compressed) -> decompress(Compressed, "").

decompress(Compressed, Expanded) -> 
    Data = re:split(Compressed, "[(x)]", [{return, list}, {parts, 4}]),
    case contains_marker(Data) of
        false -> Expanded ++ Compressed;
        true -> {Prefix, Decompressed, Suffix} = expand(Data),
                decompress(Suffix, Expanded ++ Prefix ++ Decompressed)
    end.
