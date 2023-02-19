-module(accumulate).
-export([accumulate/2]).

%%
%% given a fun and a list, apply fun to each list item replacing list item with fun's return value.
%%
-spec accumulate(fun((A) -> B), list(A)) -> list(B).
accumulate(_Fn, _List) ->
    lists:reverse(accumulate(_Fn, _List, [])).

accumulate(_, [], _Acc) ->
    _Acc;
accumulate(_Fn, [_H|_T], Acc) ->
    accumulate(_Fn, _T, [_Fn(_H)|Acc]).

