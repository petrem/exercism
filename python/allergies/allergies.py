class Allergies(object):
    ALLERGENS = [
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats"
    ]

    def __init__(self, score):
        self._score = score & 0xff

    def _check(self, index):
        return self._score & 1 << index

    def allergic_to(self, item):
        return bool(self._check(self.ALLERGENS.index(item)))

    @property
    def lst(self):
        return [self.ALLERGENS[i] for i in range(8) if self._check(i)]
