convert(N, Numeral):-
    digits(N, Digits),
    reverse(Digits, ReversedDigits),
    roman_digits(ReversedDigits, [units, tens, hundreds, thousands], ReversedRomanDigits),
    reverse(ReversedRomanDigits, RomanDigits),
    concatenate_strings(RomanDigits, Numeral).

digits(N, Digits):-
    number_to_chars(N, Chars),
    chars_to_digits(Chars, Digits).

chars_to_digits([], []).
chars_to_digits([C|Cs], [D|Ds]):-
    char_type(C, digit(D)),
    chars_to_digits(Cs, Ds).

roman_digits([], _, []).
roman_digits([D|Ds], [P|Ps], [Rd|Rds]):-
    roman_digit(P, D, Rd),
    !,
    roman_digits(Ds, Ps, Rds).

roman_digit(_, 0, "").

roman_digit(units, 1, "I").
roman_digit(units, 2, "II").
roman_digit(units, 3, "III").
roman_digit(units, 4, "IV").
roman_digit(units, 5, "V").
roman_digit(units, 6, "VI").
roman_digit(units, 7, "VII").
roman_digit(units, 8, "VIII").
roman_digit(units, 9, "IX").

roman_digit(tens, 1, "X").
roman_digit(tens, 2, "XX").
roman_digit(tens, 3, "XXX").
roman_digit(tens, 4, "XL").
roman_digit(tens, 5, "L").
roman_digit(tens, 6, "LX").
roman_digit(tens, 7, "LXX").
roman_digit(tens, 8, "LXXX").
roman_digit(tens, 9, "XC").

roman_digit(hundreds, 1, "C").
roman_digit(hundreds, 2, "CC").
roman_digit(hundreds, 3, "CCC").
roman_digit(hundreds, 4, "CD").
roman_digit(hundreds, 5, "D").
roman_digit(hundreds, 6, "DC").
roman_digit(hundreds, 7, "DCC").
roman_digit(hundreds, 8, "DCCC").
roman_digit(hundreds, 9, "CM").

roman_digit(thousands, 1, "M").
roman_digit(thousands, N, Digit):-
    N1 is N - 1,
    roman_digit(thousands, N1, Prev),
    !,
    string_concat("M", Prev, Digit).

concatenate_strings(StringList, Concatenated) :-
    maplist(atom_chars, StringList, Lists),
    append(Lists, List),
    string_chars(Concatenated, List).
