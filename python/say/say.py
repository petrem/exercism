def say(number):
    if not (0 <= number <= MAX):
        raise ValueError("Cannot say negative or large numbers: I'm dumb or unschooled")
    return " ".join(
        reversed(
            list(
                f"{say_hundreds(digits)}" + (f" {power}" if power else "")
                for digits, power in zip(
                    groups_of_digits_reversed(number), POWERS_NUMERALS
                )
                if digits != "000"
            )
        )
    )


def groups_of_digits(number):
    digits = f"{number}"
    last = len(digits)
    end = (last + 2) % 3 + 1
    yield digits[:end]
    while end < last:
        end += 3
        yield digits[end - 3:end]


def groups_of_digits_reversed(number):
    digits = f"{number}"
    end = len(digits)
    while end > 0:
        yield digits[max(0, end - 3): end]
        end -= 3


def say_hundreds(digits):
    if len(digits) == 3 and digits[0] != "0":
        hundreds = f"{NUMERALS[int(digits[0])]} hundred"
        rest = int(digits[1:])
        if rest == 0:
            return hundreds
        hundreds += " and "
    else:
        hundreds = ""
        rest = int(digits)

    if rest < 13:
        tens = say_upto_dozen(rest)
    elif rest < 20:
        tens = say_teens(rest)
    else:
        tens = say_tens(rest)
    return hundreds + tens


def say_upto_dozen(number):
    return NUMERALS[number]


def say_teens(number):
    last_digit = number % 10
    prefix = TEEN_PREFIXES.get(last_digit, PREFIXES[last_digit])
    return f"{prefix}teen"


def say_tens(number):
    last_digit = number % 10
    return PREFIXES[number // 10] + "ty" + (
        ("-" + say_upto_dozen(last_digit)) if last_digit else ""
    )


MAX = 999999999999

TEEN_PREFIXES = {
    4: "four",
}

PREFIXES = {
    2: "twen",
    3: "thir",
    4: "for",
    5: "fif",
    6: "six",
    7: "seven",
    8: "eigh",
    9: "nine",
}

NUMERALS = {
    0: "zero",
    1: "one",
    2: "two",
    3: "three",
    4: "four",
    5: "five",
    6: "six",
    7: "seven",
    8: "eight",
    9: "nine",
    10: "ten",
    11: "eleven",
    12: "twelve",
}

POWERS_NUMERALS = [None, "thousand", "million", "billion"]
