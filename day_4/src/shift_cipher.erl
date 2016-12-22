-module(shift_cipher).
-export([decrypt/2]).

decrypt(Code, ShiftAmount) when is_list(Code) -> 
    lists:map(fun(X) -> decrypt(X, ShiftAmount) end, Code);
decrypt($-, _) ->
    $ ;
decrypt(Letter, ShiftAmount) ->
    ((Letter - $a + ShiftAmount) rem 26) + $a.
    

