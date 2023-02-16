-module(secret_handshake).

-export([commands/1]).

commands(_Number) ->
    RegularActions = [ {2#00001, "wink"}
                     , {2#00010, "double blink"}
                     , {2#00100, "close your eyes"}
                     , {2#01000, "jump"}],
    Reverse = 2#10000,
    PerformedActions = [_Action || {_Bit, _Action} <- RegularActions, has_bit(_Number, _Bit)],
    case has_bit(_Number, Reverse) of
       false ->
            PerformedActions;
       true ->
            lists:reverse(PerformedActions)
    end.

has_bit(_Number, _Bit) ->
    _Number band _Bit /= 0.
