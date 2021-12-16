from collections import Counter


def find_anagrams(word, candidates):
    _word = word.lower()
    _letters = Counter(_word)

    def is_anagram(candidate):
        return candidate != _word and Counter(candidate) == _letters

    return [c for c in candidates if is_anagram(c.lower())]
