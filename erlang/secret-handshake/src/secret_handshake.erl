-module(secret_handshake).

-export([commands/1]).

commands(_Number) ->
    Bits = [2#00001, 2#00010, 2#00100, 2#01000, 2#10000],
    commands(Bits, _Number, []).

commands([], _, _Acc) ->
    lists:reverse(_Acc);
commands([_Bit | _Rest], _Number, _Acc) when _Bit band _Number /= 0 ->
    commands(_Rest
            , _Number
            , case _Bit of
                  2#00001 -> ["wink" | _Acc];
                  2#00010 -> ["double blink" | _Acc];
                  2#00100 -> ["close your eyes" | _Acc];
                  2#01000 -> ["jump" | _Acc];
                  2#10000 -> lists:reverse(_Acc)
              end
            );
commands([_Bit | _Rest], _Number, _Acc) when _Bit band _Number == 0 ->
    commands(_Rest, _Number, _Acc).
                                
           
                                      
            
