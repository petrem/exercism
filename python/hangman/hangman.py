# Game status categories
# Change the values as you see fit
STATUS_WIN = "won"
STATUS_LOSE = "lost"
STATUS_ONGOING = "ongoing"


class Hangman(object):
    def __init__(self, word):
        self.remaining_guesses = 9
        self.status = STATUS_ONGOING
        self.guessed = set()
        self._word = word

    def guess(self, char):
        if self.status != STATUS_ONGOING:
            raise ValueError(f"Cannot guess anymore, the game is {self.status}")
        if char in self._word and char not in self.guessed:
            self.guessed.add(char)
            if set(self._word) == self.guessed:
                self.status = STATUS_WIN
        else:
            self.remaining_guesses -= 1
            if self.remaining_guesses == -1:
                self.status = STATUS_LOSE

    def get_masked_word(self):
        return "".join(c if c in self.guessed else "_" for c in self._word)

    def get_status(self):
        return self.status
