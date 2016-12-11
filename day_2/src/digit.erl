-module(digit).
-export([decode_instructions/1, decode_instructions/2]).

decode_instructions(Digit, []) -> Digit;
decode_instructions(Digit, [Instruction|Rest]) -> decode_instructions(keypad:map(Digit, Instruction), Rest). 

decode_instructions(Instructions) -> decode_instructions(5, "", Instructions).

decode_instructions(Digit, [], []) -> integer_to_list(Digit);
decode_instructions(_, Code, []) -> Code;
decode_instructions(Digit, Code, [Instruction|Rest]) -> 
    NextDigit = decode_instructions(Digit, Instruction),
    decode_instructions(NextDigit, Code ++ integer_to_list(NextDigit), Rest).
