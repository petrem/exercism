-module(strain).

-export([keep/2, discard/2]).

keep(_Fn, _List) -> [X || X <- _List, _Fn(X)].

discard(_Fn, _List) -> [X || X <- _List, not(_Fn(X))].
