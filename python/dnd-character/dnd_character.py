import random


def modifier(c):
    return (c - 10) // 2


class Character:
    ABILITIES = (
        "strength", "dexterity", "constitution", "intelligence", "wisdom", "charisma"
    )

    def __init__(self):
        for attr in self.ABILITIES:
            setattr(self, attr, self.ability())
        setattr(self, "hitpoints", 10 + modifier(self.constitution))

    def ability(self):
        return sum(sorted(random.choices(range(1, 7), k=4))[1:])
