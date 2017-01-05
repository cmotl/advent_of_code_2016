-module(decompressor3).
-export([decompress/1]).

contains_marker([_,_,_,_]) -> true;
contains_marker([_]) -> false.

expand([Prefix, SizeString, CountString, Suffix]) -> 
    Size = list_to_integer(SizeString),
    Count = list_to_integer(CountString),
    Repeat = string:substr(Suffix, 1, Size),
    ExpandedRepeatCount = decompress(Repeat),
    Remainder = string:substr(Suffix, 1+Size),
    DecompressedSize = Count * ExpandedRepeatCount,
    {length(Prefix), DecompressedSize, Remainder}.

decompress(Compressed) -> decompress(Compressed, 0).

decompress(Compressed, Expanded) -> 
    Data = re:split(Compressed, "[(x)]", [{return, list}, {parts, 4}]),
    case contains_marker(Data) of
        false -> Expanded + length(Compressed);
        true -> {Prefix, Decompressed, Suffix} = expand(Data),
                decompress(Suffix, Expanded + Prefix + Decompressed)
    end.
