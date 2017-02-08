-module(scramble).
-export([parse/1, swap/2, move/2, reverse/2, rotate/2, unrotate/2, execute/2, invert_instruction/1]).

parse(["swap", "position", Source, "with", "position", Destination]) ->
    {swap, position, list_to_integer(Source), list_to_integer(Destination)};
parse(["swap", "letter", Source, "with", "letter", Destination]) ->
    {swap, letter, list_to_atom(Source), list_to_atom(Destination)};

parse(["rotate", "left", Number, "step"]) ->
    {rotate, left, list_to_integer(Number)};
parse(["rotate", "left", Number, "steps"]) ->
    {rotate, left, list_to_integer(Number)};
parse(["rotate", "right", Number, "step"]) ->
    {rotate, right, list_to_integer(Number)};
parse(["rotate", "right", Number, "steps"]) ->
    {rotate, right, list_to_integer(Number)};
parse(["rotate", "based", "on", "position", "of", "letter", Letter]) ->
    {rotate, letter, list_to_atom(Letter)};
   
parse(["reverse", "positions", From, "through", To]) -> 
    {reverse, list_to_integer(From), list_to_integer(To)};

parse(["move", "position", From, "to", "position", To]) ->
    {move, list_to_integer(From), list_to_integer(To)};

parse(Instruction) ->
    Tokens = string:tokens(Instruction, " "),
    parse(Tokens).

invert_instruction({swap, position, From, To}) ->
    {swap, position, To, From};
invert_instruction({swap, letter, From, To}) ->
    {swap, letter, To, From};
invert_instruction({rotate, left, Amount}) ->
    {rotate, right, Amount};
invert_instruction({rotate, right, Amount}) ->
    {rotate, left, Amount};
invert_instruction({rotate, letter, Letter}) ->
    {unrotate, letter, Letter};
invert_instruction({move, From, To}) ->
    {move, To, From};
invert_instruction(Instruction) -> Instruction.


index_of(Value, List) ->
   Map = lists:zip(List, lists:seq(0, length(List)-1)),
   case lists:keyfind(Value, 1, Map) of
      {Value, Index} -> Index;
      false -> notfound
   end.

swap(Input, {swap, letter, From, To}) ->
    FromIndex = index_of(hd(atom_to_list(From)), Input),
    ToIndex = index_of(hd(atom_to_list(To)), Input),
    swap(Input, {swap, position, FromIndex, ToIndex});
swap(Input, {swap, position, From, To}) when (From > To) ->
    swap(Input, {swap, position, To, From});
swap(Input, {swap, position, From, To}) ->
    Pre = lists:sublist(Input, 1, From),
    FromLetter = lists:sublist(Input, From+1, 1),
    Center = lists:sublist(Input, From+2, To-(From+1)),
    ToLetter = lists:sublist(Input, To+1, 1),
    Post = lists:sublist(Input, To+2, (length(Input) - To + 1)),
    Pre ++ ToLetter ++ Center ++ FromLetter ++ Post. 


move(Input, {move, From, To}) when (From < To) ->
    PreFrom = lists:sublist(Input, 1, From),
    FromLetter = lists:sublist(Input, From+1, 1),
    FromToTo = lists:sublist(Input, From+2, To-From),
    PostTo = lists:sublist(Input, To+2, (length(Input) - To + 1)),
    PreFrom ++ FromToTo ++ FromLetter ++ PostTo;
move(Input, {move, From, To}) when (To < From) ->
    PreTo = lists:sublist(Input, 1, To),
    ToToFrom = lists:sublist(Input, To+1, From-To),
    FromLetter = lists:sublist(Input, From+1, 1),
    PostFrom = lists:sublist(Input, From+2, (length(Input) - From + 1)),
    PreTo ++ FromLetter ++ ToToFrom ++ PostFrom. 


reverse(Input, {reverse, From, To}) ->
    Pre = lists:sublist(Input, 1, From),
    Center = lists:sublist(Input, From+1, To-From+1),
    Post = lists:sublist(Input, To+2, (length(Input) - To + 1)),
    Pre ++ lists:reverse(Center) ++ Post. 

rotate_list(List, ShiftAmount) ->
    Length = length(List),
    SplitPosition = Length - (ShiftAmount rem Length),
    {Tail,Head} = lists:split(SplitPosition, List),
    Head ++ Tail. 

rotate_letter(Input, Amount) when (Amount >= 4) ->
    rotate_list(Input, Amount+2);
rotate_letter(Input, Amount) ->
    rotate_list(Input, Amount+1).

unrotate_letter(Input, Letter, 0) -> nil;
unrotate_letter(Input, Letter, ShiftAmount) ->
    X = rotate(Input, {rotate, left, length(Input) - ShiftAmount}),
    Unscrambled = rotate(X, {rotate, letter, Letter}),
    case Unscrambled == Input of
        true -> X;
        false -> unrotate_letter(Input, Letter, ShiftAmount-1)
    end.

rotate(Input, {rotate, letter, Letter}) ->
    Index = index_of(hd(atom_to_list(Letter)), Input),
    rotate_letter(Input, Index);
rotate(Input, {rotate, left, Amount}) ->
    lists:reverse(rotate_list(lists:reverse(Input), Amount));
rotate(Input, {rotate, right, Amount}) ->
    rotate_list(Input, Amount).

unrotate(Input, {unrotate, letter, Letter}) ->
    unrotate_letter(Input, Letter, length(Input)).    

execute(Input, []) -> Input;
execute(Input, [Instruction|Rest]) ->
    ScrambledInput = case element(1, Instruction) of 
        swap -> swap(Input, Instruction);
        reverse -> reverse(Input, Instruction);
        rotate -> rotate(Input, Instruction);
        unrotate -> unrotate(Input, Instruction);
        move -> move(Input, Instruction)
    end,
    execute(ScrambledInput, Rest).
