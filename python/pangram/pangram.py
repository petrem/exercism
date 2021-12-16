import string


alphabet = set(string.ascii_lowercase)


def is_pangram(sentence):
    letters = {letter.lower() for letter in sentence if letter.lower() in alphabet}
    return letters == alphabet


# the obligatory standard solution
def is_pangram2(sentence):
    lower_sentence = sentence.lower()
    return all(letter in lower_sentence for letter in alphabet)
