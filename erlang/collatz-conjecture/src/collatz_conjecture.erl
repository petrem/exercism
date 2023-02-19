-module(collatz_conjecture).

-export([steps/1]).


steps(_N) when _N > 0 -> steps(_N, 0);
steps(_) -> error(badarg). 

steps(1, _Steps) ->
    _Steps;
steps(_N, _Steps) when _N rem 2 =:= 0 ->
    steps(_N div 2, _Steps + 1);
steps(_N, _Steps) -> 
    steps(_N * 3 + 1, _Steps + 1).

