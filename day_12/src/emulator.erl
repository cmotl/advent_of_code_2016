-module(emulator).
-export([parse/1, advance_instruction_cache_by/2, execute/1, execute_instruction/2]).

parse(Instruction) ->
    [Opcode | Tokens]  = string:tokens(Instruction, " "), 
    case Opcode of
        "cpy" -> parse_cpy(Tokens);
        "inc" -> parse_inc(Tokens);
        "dec" -> parse_dec(Tokens);
        "jnz" -> parse_jnz(Tokens)
    end.

parse_cpy(["a", Destination]) -> {cpy, a, list_to_atom(Destination)};
parse_cpy(["b", Destination]) -> {cpy, b, list_to_atom(Destination)};
parse_cpy(["c", Destination]) -> {cpy, c, list_to_atom(Destination)};
parse_cpy(["d", Destination]) -> {cpy, d, list_to_atom(Destination)};
parse_cpy([Value, Destination]) -> {cpy, list_to_integer(Value), list_to_atom(Destination)}.

parse_inc([Register]) -> {inc, list_to_atom(Register)}.

parse_dec([Register]) -> {dec, list_to_atom(Register)}.

parse_jnz(["a", Delta]) -> {jnz, a, list_to_integer(Delta)};
parse_jnz(["b", Delta]) -> {jnz, b, list_to_integer(Delta)};
parse_jnz(["c", Delta]) -> {jnz, c, list_to_integer(Delta)};
parse_jnz(["d", Delta]) -> {jnz, d, list_to_integer(Delta)};
parse_jnz([Value, Delta]) -> {jnz, list_to_integer(Value), list_to_integer(Delta)}.


advance_instruction_cache(InstructionCache, 0) -> InstructionCache; 
advance_instruction_cache(InstructionCache, Count) -> 
    advance_instruction_cache(advance_instruction_cache(InstructionCache), Count - 1).

advance_instruction_cache({ExecutedInstructions, [Next|Rest]}) -> 
    {[Next|ExecutedInstructions], Rest}.

rewind_instruction_cache(InstructionCache, 0) -> InstructionCache; 
rewind_instruction_cache(InstructionCache, Count) -> 
    rewind_instruction_cache(rewind_instruction_cache(InstructionCache), Count - 1).

rewind_instruction_cache({[Instruction|Previous], Next}) ->
    {Previous, [Instruction| Next]}. 

advance_instruction_cache_by(InstructionCache, Count) when Count > 0 ->
    advance_instruction_cache(InstructionCache, Count);
advance_instruction_cache_by(InstructionCache, Count) when Count < 0 ->
    rewind_instruction_cache(InstructionCache, Count*-1).
   

next_instruction(InstructionCache) ->
    {_, [Instruction|_]} = InstructionCache,
    Instruction.

execute_instruction({inc, Register}, {Registers, InstructionCache}) -> 
    #{Register := Value} = Registers,
    {Registers#{Register := Value + 1 }, advance_instruction_cache(InstructionCache)};

execute_instruction({dec, Register}, {Registers, InstructionCache}) -> 
    #{Register := Value} = Registers,
    {Registers#{Register := Value - 1 }, advance_instruction_cache(InstructionCache)};

execute_instruction({cpy, Value, ToRegister}, {Registers, InstructionCache}) when is_integer(Value) -> 
    {Registers#{ToRegister := Value }, advance_instruction_cache(InstructionCache)};

execute_instruction({cpy, FromRegister, ToRegister}, {Registers, InstructionCache}) when is_atom(FromRegister) -> 
    #{FromRegister := Value} = Registers,
    {Registers#{ToRegister := Value }, advance_instruction_cache(InstructionCache)};

execute_instruction({jnz, 0, _Delta}, {Registers, InstructionCache}) ->
    {Registers, advance_instruction_cache(InstructionCache)};
execute_instruction({jnz, Value, Delta}, {Registers, InstructionCache}) when is_integer(Value) ->
    {Registers, advance_instruction_cache_by(InstructionCache, Delta)};

execute_instruction({jnz, Register, Delta}, {Registers, InstructionCache}) when is_atom(Register) ->
    #{Register := Value} = Registers,
    case Value of
        0 -> {Registers, advance_instruction_cache(InstructionCache)};
        _ -> {Registers, advance_instruction_cache_by(InstructionCache, Delta)}
    end.

execute({_Registers, {_, []}} = State) -> State;
execute({_Registers, InstructionCache} = State) ->
    Instruction = next_instruction(InstructionCache),
    NewState = execute_instruction(Instruction, State),
    execute(NewState).

