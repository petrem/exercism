from itertools import compress, cycle


def get_rounds(number):
    """

    :param number: int - current round number.
    :return: list - current round and the two that follow.
    """

    return [number + i for i in range(3)]


def concatenate_rounds(rounds_1, rounds_2):
    """

    :param rounds_1: list - first rounds played.
    :param rounds_2: list - second set of rounds played.
    :return: list - all rounds played.
    """

    return rounds_1 + rounds_2


def list_contains_round(rounds, number):
    """

    :param rounds: list - rounds played.
    :param number: int - round number.
    :return:  bool - was the round played?
    """

    return number in rounds


def card_average(hand):
    """

    :param hand: list - cards in hand.
    :return:  float - average value of the cards in the hand.
    """

    return sum(hand) / len(hand)


def approx_average_is_average(hand):
    """

    :param hand: list - cards in hand.
    :return: bool - if approximate average equals to the `true average`.
    """
    true_average = card_average(hand)
    if (hand[0] + hand[-1]) / 2 == true_average:
        return True
    if hand[len(hand) // 2] == true_average:
        return True
    return False


def average_even_is_average_odd(hand):
    """

    :param hand: list - cards in hand.
    :return: bool - are even and odd averages equal?
    """

    # A convoluted method to do it: about 5 times slower, but more fun
    odd_len = len(hand) // 2
    even_len = odd_len + len(hand) % 2
    even_average = sum(compress(hand, cycle([True, False]))) / even_len
    odd_average = sum(compress(hand, cycle([False, True]))) / odd_len

    # the straight-forward way:
    even_list = hand[::2]
    odd_list = hand[1::2]

    assert sum(even_list) / len(even_list) == even_average
    assert sum(odd_list) / len(odd_list) == odd_average

    return even_average == odd_average


def maybe_double_last(hand):
    """

    :param hand: list - cards in hand.
    :return: list - hand with Jacks (if present) value doubled.
    """
    if hand[-1] == 11:
        return [*hand[:-1], 22]
    return hand
