-module(roman_numerals).

-import(array, [from_list/1]).
-import(lists, [concat/1, map/2, reverse/1]).

-export([roman/1]).

roman(_Number) when _Number =< 0 -> erlang:error(badarg, "acvila non capit negatives");
roman(_Number) when _Number > 3999 -> erlang:error(badarg, "whoa, whoa, too much");
roman(_Number) ->
    RomanDigits = [ from_list(["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"])
                  , from_list(["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"])
                  , from_list(["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"])
                  , from_list(["", "M", "MM", "MMM", "MMM"])
                  ],
    ReversedArabicDigits = reverse([X - $0 || X <- erlang:integer_to_list(_Number)]),
    concat(
      zipwith_shortest_reversed(fun array:get/2, ReversedArabicDigits, RomanDigits)).


% it's a shame we have to define a zip working on lists with different lengths
% so since we implement it ourselves let's profit and get the result reversed.
zipwith_shortest_reversed(_F, _L1, _L2) ->
    zipwith_shortest_reversed(_F, _L1, _L2, []).

zipwith_shortest_reversed(_, _, [], _R) ->
    _R;
zipwith_shortest_reversed(_, [], _, _R) ->
    _R;
zipwith_shortest_reversed(_F, [_X|_XS], [_Y|_YS], _R) ->
    zipwith_shortest_reversed(_F, _XS, _YS, [_F(_X, _Y) | _R]).


