-module(letter_frequency_proc).

-export([init/1, spawn/2, spawn_link/2]).

letter_frequencies(_String) ->
    lists:foldr( fun(_K, _Acc) -> dict:update_counter(_K, 1, _Acc) end
               , dict:new()
               , _String
               ).

    
