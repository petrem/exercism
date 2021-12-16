class Luhn:
    __slots__ = "_digits"

    def __init__(self, card_num: str) -> None:
        if not isinstance(card_num, str):
            raise ValueError("card_num must be a string")
        # Remove allowed characters to check for leftovers
        if card_num.translate(str.maketrans("", "", "0123456789 ")):
            self._digits = []
        else:
            self._digits = [int(d) for d in reversed(card_num) if d.isdigit()]

    def valid(self):
        if len(self._digits) < 2:
            return False
        luhn_sum = (
            # somewhat surprisingly, this is faster than looking up
            # in a precalculated list
            sum(d * 2 if d * 2 < 10 else d * 2 - 9 for d in self._digits[1::2])
            + sum(d for d in self._digits[::2])
        )
        return luhn_sum % 10 == 0
