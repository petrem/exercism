# bake time in oven
EXPECTED_BAKE_TIME = 40

# time it takes to prepare a single layer, in minutes
PREPARATION_TIME = 2


def bake_time_remaining(elapsed_bake_time: int) -> int:
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int baking time already elapsed.
    :return: int remaining bake time derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """

    return EXPECTED_BAKE_TIME - elapsed_bake_time


def preparation_time_in_minutes(layers: int) -> int:
    """Calculate preparation time depending on desired number of layers.

    :param layers: int desired number of layers.
    :return: int preparation time in minutes.
    """

    return PREPARATION_TIME * layers


def elapsed_time_in_minutes(number_of_layers: int, elapsed_bake_time: int) -> int:
    """Return elapsed cooking time.

    :param number_of_layers: int number of lasagna layers.
    :param elapsed_bake_time: int bake time so far.

    This function takes two numbers representing the number of layers & the time already
    spent baking and calculates the total elapsed minutes spent cooking the lasagna.

    """

    return preparation_time_in_minutes(number_of_layers) + elapsed_bake_time
