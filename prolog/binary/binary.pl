binary(Str, Dec):-
    string_to_01_list(Str, ZOL),
    reverse(ZOL, Rev),
    binary_helper(Rev, Dec).

binary_helper([], 0).
binary_helper([D|T], Rez):-
    binary_helper(T, Dec), Rez is 2 * Dec + D.

string_to_01_list(Str, L4):-
    string_codes(Str, L2),
    codes_to_01s(L2, L3),
    skip_leading_zeros(L3, L4).

codes_to_01s([], []).
codes_to_01s([H|T], [HRez|TRez]):-
    binary_char(H, HRez),
    codes_to_01s(T, TRez).
    
binary_char(48, 0).
binary_char(49, 1).

skip_leading_zeros([], []).
skip_leading_zeros([0|T], Rez):-
    skip_leading_zeros(T, Rez).
skip_leading_zeros([1|T], [1|T]).



