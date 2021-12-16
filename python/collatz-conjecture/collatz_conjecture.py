def collatz_steps(number):
    count = 0
    if number <= 0:
        raise ValueError("Will only run for nonzero natural numbers")
    while number > 1:
        if number % 2 == 0:
            number = number // 2
            count += 1
        else:
            number = (3 * number + 1) // 2
            count += 2
    return count
