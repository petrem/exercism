-module(parallel_letter_frequency_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").

single_test() ->
    ?assertEqual(
        parallel_letter_frequency:dict(["asd"]),
        dict:from_list([{$a, 1}, {$d, 1}, {$s, 1}])
    ).

double_test() ->
    ?assertEqual(
        parallel_letter_frequency:dict(["asd", "asd"]),
        dict:from_list([{$a, 2}, {$d, 2}, {$s, 2}])
    ).

multi_test() ->
    ?assertEqual(
        parallel_letter_frequency:dict(lists:duplicate(10000, "asd")),
        dict:from_list([{$a, 10000}, {$d, 10000}, {$s, 10000}])
    ).
