-module(parallel_letter_frequency).

-export([dict/1]).


dict(Strings) ->
    Workers = [start_worker(String) || String <- Strings],
    lists:foldl(fun(Pid, Acc) ->
                        Freqs = gather_worker(Pid),
                        dict:merge(fun(_K, V1, V2) ->
                                           V1 + V2 end, Freqs, Acc)
                end,
                dict:new(),
                Workers).

start_worker(String) ->
    Parent = self(),
    spawn_link(fun() ->
                       Parent ! {self(), {reply, letter_frequencies(String)}}
               end).

gather_worker(Pid) ->
    receive {Pid, {reply, Result}} -> Result end.
                                  
    

letter_frequencies(_String) ->
    lists:foldr( fun(_K, _Acc) -> dict:update_counter(_K, 1, _Acc) end
               , dict:new()
               , _String
               ).

    
