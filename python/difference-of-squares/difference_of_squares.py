def square_of_sum(count):
    return sum(range(1, count + 1)) ** 2


def sum_of_squares(count):
    return sum(i ** 2 for i in range(1, count + 1))


def difference(count):
    _sum = 0
    half_diff = 0
    for i in range(1, count):
        _sum += i
        half_diff += _sum * (i + 1)
    return 2 * half_diff


# solutions from https://exercism.io/tracks/python/exercises/difference-of-squares/solutions/272b31053b344b42b213f05635b34ff3

def square_of_sum1(count):
    return ((count+1)*count/2)**2


def sum_of_squares1(count):
    return (count*(count+1)*(2*count+1))/6


def difference1(count):
    return square_of_sum1(count)-sum_of_squares1(count)
