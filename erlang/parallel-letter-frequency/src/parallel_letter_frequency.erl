-module(parallel_letter_frequency).

-export([dict/1, letter_frequencies/1]).

%% first, a non-parallel solution (and a few more tests)


dict(_Strings) ->
    lists:foldr( fun(_S, _Acc) ->
                         dict:merge(fun(_, _V1, _V2) -> _V1 + _V2 end
                                   , letter_frequencies(_S)
                                   , _Acc
                                   )
                 end
               , dict:new()
               , _Strings
             ).

letter_frequencies(_String) ->
    lists:foldr( fun(_K, _Acc) -> dict:update_counter(_K, 1, _Acc) end
               , dict:new()
               , _String
               ).

    
