def is_armstrong(number):
    num_s = f"{number}"
    digits = len(num_s)
    return number == sum(int(d) ** digits for d in num_s)
