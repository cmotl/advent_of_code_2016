-module(factory).
-export([parse/1,send_directive_to_bot/2, send_input_to_bot/2, pending_actions/1, route_instructions_to_bots/1]).


parse(["value", Chip, "goes", "to", "bot", Bot]) -> {bot, list_to_integer(Bot), list_to_integer(Chip)};
parse(["bot", Bot, "gives", "low", "to", "bot", Low, "and", "high", "to", "bot", High]) ->
    {list_to_integer(Bot), 
        #{ low  => {bot, list_to_integer(Low)}, 
           high => {bot, list_to_integer(High)}}};
parse(["bot", Bot, "gives", "low", "to", "output", Low, "and", "high", "to", "bot", High]) ->
    {list_to_integer(Bot), 
        #{ low  => {output, list_to_integer(Low)}, 
           high => {bot, list_to_integer(High)}}};
parse(["bot", Bot, "gives", "low", "to", "bot", Low, "and", "high", "to", "output", High]) ->
    {list_to_integer(Bot), 
        #{ low  => {bot, list_to_integer(Low)}, 
           high => {output, list_to_integer(High)}}};
parse(["bot", Bot, "gives", "low", "to", "output", Low, "and", "high", "to", "output", High]) ->
    {list_to_integer(Bot), 
        #{ low  => {output, list_to_integer(Low)}, 
           high => {output, list_to_integer(High)}}};
parse(Instruction) ->
    Directions = re:split(Instruction, " ", [{return,list}]),
    parse(Directions).

send_directive_to_bot(Bot, Directive) ->
   maps:put(directive, Directive, Bot). 

send_input_to_bot(Bot, Input) ->
    maps:update_with(input, fun(V) -> [Input|V] end, [Input], Bot).

pending_actions(not_present, _) -> [];
pending_actions(_, not_present) -> [];
pending_actions(_, [_]) -> [];
pending_actions(Directive, Inputs) ->
    {LowDestination, LowId} = maps:get(low, Directive),
    {HighDestination, HighId} = maps:get(high, Directive),
    [LowInput, HighInput] = lists:sort(Inputs),
    [{LowDestination, LowId, LowInput}, {HighDestination, HighId, HighInput}].
pending_actions(Bot) -> 
    Directive = maps:get(directive, Bot, not_present),
    Inputs = maps:get(input, Bot, not_present),
    pending_actions(Directive, Inputs).

bot_id({Destination, Bot, _}) -> {Destination, Bot};
bot_id({Bot, _}) -> {bot, Bot}.

executing_bot(Instruction, Bots) ->
    BotId = bot_id(Instruction),
    BotExists = maps:is_key(BotId, Bots),
    case BotExists of
        false -> NewBots = maps:put(BotId, #{}, Bots), {NewBots, #{}, BotId};
        true -> {Bots, maps:get(BotId, Bots), BotId}
    end.

route_instruction_to_bot({_, Directive}, Bot) ->
    send_directive_to_bot(Bot, Directive);
route_instruction_to_bot({_,_,Value}, Bot) ->
    send_input_to_bot(Bot, Value).

route_instructions_to_bots([], Bots, Actions) -> {Bots, Actions};
route_instructions_to_bots([Instruction|Rest], Bots, Actions) ->
    {NewBots, ExecutingBot, BotId} = executing_bot(Instruction, Bots),
    UpdatedBot = route_instruction_to_bot(Instruction, ExecutingBot),
    UpdatedBots = maps:update(BotId, UpdatedBot, NewBots),
    PendingActions = pending_actions(UpdatedBot),
    {AgainUpdatedBots, UpdatedActions} = route_instructions_to_bots(PendingActions, UpdatedBots, Actions),
    NewActions = 
        case PendingActions of
            [] -> UpdatedActions;
            _ -> [{BotId, PendingActions}|UpdatedActions]
        end,
    route_instructions_to_bots(Rest, AgainUpdatedBots, NewActions).
    
route_instructions_to_bots(Instructions) ->
    {_, Actions} = route_instructions_to_bots(Instructions, #{}, []),
    Actions.
    
