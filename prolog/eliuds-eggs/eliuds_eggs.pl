:- use_module(library(clpfd)).

egg_count(0, 0):- !.
egg_count(EncodedCount, Count):-
    NewEncoded is EncodedCount >> 1,
    egg_count(NewEncoded, NewCount),
    Count is NewCount + (EncodedCount /\ 1).
