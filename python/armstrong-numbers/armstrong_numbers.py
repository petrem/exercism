def is_armstrong_number(number):
    num_s = str(number)
    digits = len(num_s)
    return number == sum(int(d) ** digits for d in num_s)
