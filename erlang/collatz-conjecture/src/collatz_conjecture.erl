-module(collatz_conjecture).

-export([steps/1]).

%% A faster version. Tested on a randomly typed number of over 8300 digits (ok,
%% copy-pasting parts of it) that takes 198746 steps, this looks to be about 4 time
%% faster, even with a very inefficient way to get the number of trailing zero bits.

steps(_N) when _N > 0 -> steps(_N, 0);
steps(_) -> error(badarg).

steps(1, _Steps) ->
    _Steps;
steps(_N, _Steps) when _N rem 2 =:= 0 ->
    {_EvenSteps, _Rest} = make_odd(_N),
    steps(_Rest, _Steps + _EvenSteps);
steps(_N, _Steps) ->
    {_OddSteps, _Rest} = hailstone(_N),
    steps(_Rest, _Steps + _OddSteps).


% _N is expected to be odd, result is always odd.
hailstone(_N) ->
    {Steps, Rest} = make_odd(_N * 3 + 1),
    {Steps + 1, Rest}.

% _N is expected to be even.
make_odd(_N) ->
    Tz = count_trailing_zeros(_N),
    {Tz, _N bsr Tz}.

% There should be a hardware implementation for CTZ, otherwise this is very
% inefficient as written here.
count_trailing_zeros(_N) ->
    count_trailing_zeros(_N, 1, 0).

count_trailing_zeros(0, _, _) ->
    0;
count_trailing_zeros(_N, _Pos, _Ctz) when _N band _Pos =/= 0 ->
    _Ctz;
count_trailing_zeros(_N, _Pos, _Ctz) ->
    count_trailing_zeros(_N, _Pos bsl 1, _Ctz + 1).
