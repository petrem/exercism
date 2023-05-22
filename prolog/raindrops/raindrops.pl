convert(N, Str):-
    raindrops(N, [(3, "Pling"), (5, "Plang"), (7, "Plong")], Raindrops),
    [_|_] = Raindrops,
    concat_string_list(Raindrops, Str),
    !.
convert(N, Str):- number_string(N, Str).

raindrops(_, [], []):- !.
raindrops(N, [(X, Drop)|T], [Drop|Rs]):-
    (0 is mod(N, X)),
    raindrops(N, T, Rs),
    !.
raindrops(N, [_|T], Rs):-
    raindrops(N, T, Rs), !.

concat_string_list([], "").
concat_string_list([H|T], Rez):-
    concat_string_list(T, S),
    string_concat(H, S, Rez).

