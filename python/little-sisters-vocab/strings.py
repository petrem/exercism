from typing import List


def _prefixed(prefix: str, word: str) -> str:
    return f"{prefix}{word}"


def add_prefix_un(word: str) -> str:
    """

    :param word: str of a root word
    :return:  str of root word with un prefix

    This function takes `word` as a parameter and
    returns a new word with an 'un' prefix.
    """

    return _prefixed("un", word)


def make_word_groups(vocab_words: List[str]) -> str:
    """

    :param vocab_words: list of vocabulary words with a prefix.
    :return: str of prefix followed by vocabulary words with
             prefix applied, separated by ' :: '.

    This function takes a `vocab_words` list and returns a string
    with the prefix  and the words with prefix applied, separated
     by ' :: '.
    """

    prefix, *words = vocab_words
    return " :: ".join([prefix, *(_prefixed(prefix, w) for w in words)])


VOWELS = "aeoiuAEIOU"


def remove_suffix_ness(word: str) -> str:
    """

    :param word: str of word to remove suffix from.
    :return: str of word with suffix removed & spelling adjusted.

    This function takes in a word and returns the base word with `ness` removed.
    """

    suffixless = word.removesuffix("ness")
    if (
        word != suffixless
        and len(suffixless) > 1
        and suffixless[-2] not in VOWELS
        and suffixless[-1] == "i"
    ):
        return f"{suffixless[:-1]}y"
    return suffixless


def adjective_to_verb(sentence: str, index: int) -> str:
    """

    :param sentence: str that uses the word in sentence
    :param index:  index of the word to remove and transform
    :return:  str word that changes the extracted adjective to a verb.

    A function takes a `sentence` using the
    vocabulary word, and the `index` of the word once that sentence
    is split apart.  The function should return the extracted
    adjective as a verb.
    """

    word = "".join(filter(str.isalpha, sentence.split()[index]))
    return f"{word}en"
