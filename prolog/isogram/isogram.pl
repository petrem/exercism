isogram(Phrase):-
    string_codes(Phrase, Codes), isogram_helper(Codes, []).

isogram_helper([], _):- true.
isogram_helper([H|T], Chars):-
    not(char_type(H, alpha)),
    isogram_helper(T, Chars).
isogram_helper([H|T], Chars):-
    char_type(H, to_upper(LowerH)),
    not(member(LowerH, Chars)),
    isogram_helper(T, [LowerH|Chars]).
