-module(decompressor3).
-export([decompress/1]).

contains_marker([_,_,_]) -> true;
contains_marker([_]) -> false.

extract_marker(Marker) ->
    Delimiter = string:chr(Marker, $x),
    Size = string:substr(Marker, 1, Delimiter-1),
    Count = string:substr(Marker, 1+Delimiter),
    {list_to_integer(Size), list_to_integer(Count)}.

expand([Prefix, Marker, Suffix]) -> 
    {Size, Count} = extract_marker(Marker),
    Repeat = string:substr(Suffix, 1, Size),
    ExpandedRepeatCount = decompress(Repeat),
    Remainder = string:substr(Suffix, 1+Size),
    %Decompressed = lists:flatten(lists:duplicate(Count, ExpandedRepeat)),
    DecompressedSize = Count * ExpandedRepeatCount,
    {length(Prefix), DecompressedSize, Remainder}.

split(Compressed) ->
    StartOfMarker = string:chr(Compressed, $(),
    EndOfMarker = string:chr(Compressed, $)),
    case StartOfMarker of
        0 -> [Compressed];
        _ -> [ string:substr(Compressed, 1, StartOfMarker-1), string:substr(Compressed, 1+StartOfMarker, EndOfMarker-StartOfMarker-1), string:substr(Compressed, 1+EndOfMarker)]
    end.
    
decompress(Compressed) -> decompress(Compressed, 0).

decompress(Compressed, Expanded) -> 
    Data = split(Compressed),
    case contains_marker(Data) of
        false -> Expanded + length(Compressed);
        true -> {Prefix, Decompressed, Suffix} = expand(Data),
                decompress(Suffix, Expanded + Prefix + Decompressed)
    end.
