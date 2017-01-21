-module(factory_test).
-include_lib("eunit/include/eunit.hrl").

-import(factory, [parse/1, send_directive_to_bot/2, send_input_to_bot/2, pending_actions/1, route_instructions_to_bots/1]).

should_parse_pickup_test() -> 
    ?assertEqual({bot, 2, 5}, parse("value 5 goes to bot 2")).

should_parse_directive_to_two_bots_test() -> 
    ?assertEqual({2, #{low =>{bot, 1}, high => {bot, 0}}}, parse("bot 2 gives low to bot 1 and high to bot 0")).

should_parse_directive_to_one_bot_and_one_output_test() -> 
    ?assertEqual({2, #{low => {output, 1}, high => {bot, 0}}}, parse("bot 2 gives low to output 1 and high to bot 0")),
    ?assertEqual({2, #{low => {bot, 1}, high => {output, 0}}}, parse("bot 2 gives low to bot 1 and high to output 0")).

should_parse_directive_to_two_outputs_test() -> 
    ?assertEqual({2, #{low => {output, 1}, high => {output, 0}}}, parse("bot 2 gives low to output 1 and high to output 0")).

should_send_directive_to_bot_test() ->
    Bot = #{},
    Directive = {a},
    ExpectedBot = #{directive => Directive},
    ?assertEqual(ExpectedBot, send_directive_to_bot(Bot, Directive)).

should_send_first_input_to_bot_test() ->
    Bot = #{},
    Input = 5,
    ExpectedBot = #{input => [Input]},
    ?assertEqual(ExpectedBot, send_input_to_bot(Bot, Input)).

should_send_second_input_to_bot_test() ->
    Bot = #{input => [5]},
    Input = 6,
    ExpectedBot = #{input => [6,5]},
    ?assertEqual(ExpectedBot, send_input_to_bot(Bot, Input)).


should_get_no_pending_actions_from_bot_with_no_directive_and_no_inputs_test() ->
    Bot = #{},
    ExpectedActions = [],
    ?assertEqual(ExpectedActions, pending_actions(Bot)).

should_get_no_pending_actions_from_bot_with_no_directive_one_input_test() ->
    Bot = #{input => [5]},
    ExpectedActions = [],
    ?assertEqual(ExpectedActions, pending_actions(Bot)).

should_get_pending_actions_from_bot_with_directive_and_two_inputs_test() ->
    Bot = #{directive=>#{low => {bot,1}, high => {bot,0}}, input => [6,5]},
    ExpectedActions = [{bot, 1, 5}, {bot, 0, 6}],
    ?assertEqual(ExpectedActions, pending_actions(Bot)).

should_route_instructions_to_bots_test() ->
    Intructions = [{bot, 2, 5}, {bot, 2, 6}, {2, #{low =>{bot, 1}, high => {bot, 0}}}],
    ExpectedOutput = [{{bot, 2}, [{bot, 1, 5}, {bot, 0, 6}]}],
    ?assertEqual(ExpectedOutput, route_instructions_to_bots(Intructions)).

should_route_instructions_to_bots_with_pending_actions_test() ->
    Intructions = [
        {bot, 2, 5}, 
        {2, #{low =>{bot, 1}, high => {bot, 0}}},
        {bot, 1, 3}, 
        {1, #{low =>{output, 1}, high => {bot, 0}}},
        {0, #{low =>{output, 2}, high => {output, 0}}},
        {bot, 2, 2}
    ],
    ExpectedOutput = [
        {{bot, 2}, [{bot, 1, 2}, {bot, 0, 5}]},
        {{bot, 0}, [{output, 2, 3}, {output, 0, 5}]},
        {{bot, 1}, [{output, 1, 2}, {bot, 0, 3}]}
    ],
    ?assertEqual(ExpectedOutput, route_instructions_to_bots(Intructions)).
