roman_num = {
    1: 'I',
    5: 'V',
    10: 'X',
    50: 'L',
    100: 'C',
    500: 'D',
    1000: 'M'
}

def roman(number: int) -> str:
    str_number = str(number)
    len_number = len(str_number)
    int_begin = int(str_number[0])

    if number == 0:
        return ''

    min_difference, num_with_min_diff = min((abs(number - k), k) for k in roman_num)
    if min_difference == 0:
        return roman_num[num_with_min_diff]

    if len_number == 1:
        if number == 9 or number == 4:
            return roman(abs(number - num_with_min_diff)) + roman_num[num_with_min_diff]
        if number == 8:
            return roman_num[5] + roman(number - 5)
        else:
            return roman_num[num_with_min_diff] + roman(number - num_with_min_diff)

    if len_number == 2:
        return foo(number, int_begin, 10)
    if len_number == 3:
        return foo(number, int_begin, 100)

    if len_number == 4:
        if 3 >= int_begin >= 1:
            return roman_num[1000] * int_begin + roman(number - (1000 * int_begin))



def foo(number, int_begin, magnitude):
    # 1 to 3: I, II, III
    if int_begin <= 3:
        it = 1
        value_roman = roman_num[magnitude] * int_begin
    # 4: IV, 9: IX
    elif int_begin in (4, 9):
        it = ...
        value_roman = roman_num[magnitude] + roman_num[(int_begin + 1) * magnitude]  # "XL" / "CD"
    # 5 to 8: V, VI, VII, VIII
    else:
        it = 5
        units = int_begin - 5
        value_roman = roman_num[5 * magnitude] + roman_num[magnitude] * units

    return value_roman + roman(abs(number - int_begin * magnitude))
