-module(hamming).

-import(lists, [filter/2, zipwith/3]).

-export([distance/2]).


distance(_Strand1, _Strand2)
  when length(_Strand1) /= length(_Strand2) -> {error, badarg};
distance(_Strand1, _Strand2) -> 
    length(filter(fun (_X) -> _X end,
                  zipwith(fun (_X, _Y) -> _X /= _Y end,
                          _Strand1,
                          _Strand2))).
