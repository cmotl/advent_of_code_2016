-module(two_factor_authentication_test).
-include_lib("eunit/include/eunit.hrl").

-import(two_factor_authentication, [parseInstruction/1, create_screen/2, turn_pixel_off/2, turn_pixel_on/2, column_of_pixels_from_board/2, row_of_pixels_from_board/2, set_column_of_pixels_on_board/3, set_row_of_pixels_on_board/3, shift_list/2, execute_instruction/2]).

should_parse_rect_instruction_test() -> 
    Instruction1 = "rect 1x2",
    ?assertEqual({rect, {1,2}}, parseInstruction(Instruction1)),

    Instruction2 = "rect 2x1",
    ?assertEqual({rect, {2,1}}, parseInstruction(Instruction2)).

should_parse_rotate_column_instruction_test() -> 
    Instruction1 = "rotate column x=1 by 2",
    ?assertEqual({rotate_column, {1,2}}, parseInstruction(Instruction1)).

should_parse_rotate_row_instruction_test() -> 
    Instruction1 = "rotate row y=2 by 1",
    ?assertEqual({rotate_row, {2,1}}, parseInstruction(Instruction1)).


should_create_a_screen_test() ->
    ExpectedScreen = dict:from_list([{{0,0}, off}]),
    ActualScreen = create_screen(1, 1),
    ?assertEqual(ExpectedScreen, ActualScreen).

should_create_a_screen_with_1_row_and_2_columns_test() ->
    ExpectedScreen = dict:from_list([{{0,0}, off}, {{0,1}, off}]),
    ActualScreen = create_screen(1, 2),
    ?assertEqual(ExpectedScreen, ActualScreen).

should_create_a_screen_with_2_row_and_1_columns_test() ->
    ExpectedScreen = dict:from_list([{{0,0}, off}, {{1,0}, off}]),
    ActualScreen = create_screen(2, 1),
    ?assertEqual(ExpectedScreen, ActualScreen).

should_create_a_screen_with_2_rows_and_2_columns_test() ->
    ExpectedScreen = dict:from_list([{{0,0},off}, {{0,1},off}, {{1,0},off}, {{1,1},off}]),
    ActualScreen = create_screen(2, 2),
    ?assertEqual(ExpectedScreen, ActualScreen).


should_turn_on_pixel_on_screen_test() ->
    Pixel = {0,0},
    ExpectedScreen = dict:from_list([{Pixel, on}]),
    Board = dict:from_list([{Pixel, off}]),
    ActualScreen = turn_pixel_on(Pixel, Board),
    ?assertEqual(ExpectedScreen, ActualScreen).

should_turn_off_pixel_on_screen_test() ->
    Pixel = {0,0},
    ExpectedScreen = dict:from_list([{Pixel, off}]),
    Board = dict:from_list([{Pixel, on}]),
    ActualScreen = turn_pixel_off(Pixel, Board),
    ?assertEqual(ExpectedScreen, ActualScreen).


should_get_row_of_pixels_from_board_test() ->
    ExpectedPixels = [on, off, on],
    Board = dict:from_list([{{0,0},on},{{0,1},off},{{0,2},on},{{1,0},off}]),
    ActualPixels = row_of_pixels_from_board(0, Board),
    ?assertEqual(ExpectedPixels, ActualPixels).

should_get_column_of_pixels_from_board_test() ->
    ExpectedPixels = [on, off, on],
    Board = dict:from_list([{{0,0},on},{{1,0},off},{{2,0},on},{{0,1},off}]),
    ActualPixels = column_of_pixels_from_board(0, Board),
    ?assertEqual(ExpectedPixels, ActualPixels).



should_set_row_of_pixels_on_board_test() ->
    Pixels = [on, off, on],
    ExpectedBoard = dict:from_list([{{0,0},on},{{0,1},off},{{0,2},on},{{1,0},off}]),
    Board = dict:from_list([{{0,0},off},{{0,1},off},{{0,2},off},{{1,0},off}]),
    ActualBoard = set_row_of_pixels_on_board(0, Pixels, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).

should_set_column_of_pixels_on_board_test() ->
    Pixels = [on, off, on],
    ExpectedBoard = dict:from_list([{{0,0},on},{{1,0},off},{{2,0},on},{{0,1},off}]),
    Board = dict:from_list([{{0,0},off},{{1,0},off},{{2,0},off},{{0,1},off}]),
    ActualBoard = set_column_of_pixels_on_board(0, Pixels, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).


should_shift_empty_list_test() ->
    ExpectedList = [],
    ActualList = shift_list([], 1),
    ?assertEqual(ExpectedList, ActualList).

should_shift_list_by_1_test() ->
    ExpectedList = [5,1,2,3,4],
    ActualList = shift_list([1,2,3,4,5], 1),
    ?assertEqual(ExpectedList, ActualList).

should_shift_list_by_more_than_the_length_of_the_list_test() ->
    ExpectedList = [4,5,1,2,3],
    ActualList = shift_list([1,2,3,4,5], 7),
    ?assertEqual(ExpectedList, ActualList).


should_execute_rect_instruction_test() ->
    Instruction = {rect, {1,1}},
    Board = create_screen(2,2),
    ExpectedBoard = dict:from_list([{{0,0},on},{{0,1},off},{{1,0},off},{{1,1},off}]),
    ActualBoard = execute_instruction(Instruction, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).

should_execute_large_rect_instruction_test() ->
    Instruction = {rect, {2,2}},
    Board = create_screen(2,2),
    ExpectedBoard = dict:from_list([{{0,0},on},{{0,1},on},{{1,0},on},{{1,1},on}]),
    ActualBoard = execute_instruction(Instruction, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).

should_execute_rotate_row_instruction_test() ->
    Instruction = {rotate_row, {0,1}},
    Board = dict:from_list([{{0,0},on},{{0,1},off},{{1,0},off},{{1,1},off}]),
    ExpectedBoard = dict:from_list([{{0,0},off},{{0,1},on},{{1,0},off},{{1,1},off}]),
    ActualBoard = execute_instruction(Instruction, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).

should_execute_rotate_column_instruction_test() ->
    Instruction = {rotate_column, {0,1}},
    Board = dict:from_list([{{0,0},on},{{0,1},off},{{1,0},off},{{1,1},off}]),
    ExpectedBoard = dict:from_list([{{0,0},off},{{0,1},off},{{1,0},on},{{1,1},off}]),
    ActualBoard = execute_instruction(Instruction, Board),
    ?assertEqual(ExpectedBoard, ActualBoard).
