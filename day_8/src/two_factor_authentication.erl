-module(two_factor_authentication).
-export([parseInstruction/1, create_screen/2, turn_pixel_on/2, turn_pixel_off/2, column_of_pixels_from_board/2, row_of_pixels_from_board/2, set_row_of_pixels_on_board/3, set_column_of_pixels_on_board/3, shift_list/2, execute_instruction/2]).

parse_rect(Dimensions) ->
    [Width, Height|[]] = re:split(Dimensions, "x", [{return, list}]),
    {rect, {list_to_integer(Width), list_to_integer(Height)}}.    

parse_index_and_shift_amount([AxisAndIndex, "by", ShiftAmount]) ->
    [_Axis, Index] = re:split(AxisAndIndex, "=", [{return, list}]),
    {list_to_integer(Index), list_to_integer(ShiftAmount)}.

parse_rotate(["row"|Rest]) ->
    {rotate_row, parse_index_and_shift_amount(Rest)};
parse_rotate(["column"|Rest]) ->
    {rotate_column, parse_index_and_shift_amount(Rest)}.

parseInstruction(Instruction) ->
    [InstructionType|Rest] = re:split(Instruction, " ", [{return, list}]),
    case InstructionType of
        "rect" -> parse_rect(Rest);
        "rotate" -> parse_rotate(Rest)
    end.

create_screen(Rows, Columns) ->
    Pixels = [{{X,Y}, off} || Y <- lists:seq(0, Columns-1), X <- lists:seq(0, Rows-1)],
    dict:from_list(Pixels).

turn_pixel_off(Pixel, Board) -> 
    dict:update(Pixel, fun(_) -> off end, Board).

turn_pixel_on(Pixel, Board) -> 
    dict:update(Pixel, fun(_) -> on end, Board).

row_of_pixels_from_board(Row, Board) ->
    PixelRow = dict:to_list(dict:filter(fun({R,_},_) -> R == Row end, Board)),
    ColumnsAndStates = lists:map(fun({{_,C},State}) -> {C,State} end, PixelRow),
    SortedColumnsAndStates = lists:sort(fun({C1,_}, {C2,_}) -> C1 < C2 end, ColumnsAndStates),
    lists:map(fun({_,V}) -> V end, SortedColumnsAndStates).

column_of_pixels_from_board(Column, Board) ->
    PixelColumn = dict:to_list(dict:filter(fun({_,C},_) -> C == Column end, Board)),
    RowsAndStates = lists:map(fun({{R,_},State}) -> {R,State} end, PixelColumn),
    SortedRowsAndStates = lists:sort(fun({R1,_}, {R2,_}) -> R1 < R2 end, RowsAndStates),
    lists:map(fun({_,V}) -> V end, SortedRowsAndStates).

set_row_of_pixels_on_board(Row, Pixels, InitialBoard) ->
    PixelRow = dict:to_list(dict:filter(fun({R,_},_) -> R == Row end, InitialBoard)),
    SortedRow = lists:sort(fun({{_,C1},_}, {{_,C2},_}) -> C1 < C2 end, PixelRow),
    FilteredSortedRow = lists:map(fun({Pixel,_}) -> Pixel end, SortedRow),
    PixelsWithUpdatedStates = lists:zip(FilteredSortedRow, Pixels),
    lists:foldl(fun({Pixel, State}, Board) -> case State of on -> turn_pixel_on(Pixel, Board); off -> turn_pixel_off(Pixel, Board) end end, InitialBoard, PixelsWithUpdatedStates).

set_column_of_pixels_on_board(Column, Pixels, InitialBoard) ->
    PixelColumn = dict:to_list(dict:filter(fun({_,C},_) -> C == Column end, InitialBoard)),
    SortedColumn = lists:sort(fun({{R1,_},_}, {{R2,_},_}) -> R1 < R2 end, PixelColumn),
    FilteredSortedColumn = lists:map(fun({Pixel,_}) -> Pixel end, SortedColumn),
    PixelsWithUpdatedStates = lists:zip(FilteredSortedColumn, Pixels),
    lists:foldl(fun({Pixel, State}, Board) -> case State of on -> turn_pixel_on(Pixel, Board); off -> turn_pixel_off(Pixel, Board) end end, InitialBoard, PixelsWithUpdatedStates).



shift_list(List, ShiftAmount) when length(List) == 0 -> List;
shift_list(List, ShiftAmount) ->
    Length = length(List),
    SplitPosition = Length - (ShiftAmount rem Length),
    {Tail,Head} = lists:split(SplitPosition, List),
    Head ++ Tail.

execute_instruction({rect,{Height, Width}}, InitialBoard) -> 
    PixelsToTurnOn = [{X,Y} || X<-lists:seq(0, Width-1), Y<-lists:seq(0, Height-1)],
    lists:foldl(fun(Pixel, Board) -> turn_pixel_on(Pixel, Board)  end, InitialBoard, PixelsToTurnOn);
execute_instruction({rotate_row, {Row, ShiftAmount}}, InitialBoard) ->
    Pixels = row_of_pixels_from_board(Row, InitialBoard),
    ShiftedPixels = shift_list(Pixels, ShiftAmount),
    set_row_of_pixels_on_board(Row, ShiftedPixels, InitialBoard);
execute_instruction({rotate_column, {Column, ShiftAmount}}, InitialBoard) ->
    Pixels = column_of_pixels_from_board(Column, InitialBoard),
    ShiftedPixels = shift_list(Pixels, ShiftAmount),
    set_column_of_pixels_on_board(Column, ShiftedPixels, InitialBoard).
    
