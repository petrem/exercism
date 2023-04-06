from itertools import chain


class Utterance(str):
    is_last = False

    def utter(self):
        return self


class Feature(Utterance):
    def utter(self):
        return f"It {self}."


class Last(Utterance):
    is_last = True


UTTERANCES = {
    "fly": Last("I don't know why she swallowed the fly. Perhaps she'll die."),
    "spider": Feature("wriggled and jiggled and tickled inside her"),
    "bird": Utterance("How absurd to swallow a bird!"),
    "cat": Utterance("Imagine that, to swallow a cat!"),
    "dog": Utterance("What a hog, to swallow a dog!"),
    "goat": Utterance("Just opened her throat and swallowed a goat!"),
    "cow": Utterance("I don't know how she swallowed a cow!"),
    "horse": Last("She's dead, of course!"),
}
ANIMALS = list(UTTERANCES)


def make_stanza(index):
    animal = ANIMALS[index]
    yield f"I know an old lady who swallowed a {animal}."
    current, utterance = animal, UTTERANCES[animal]
    yield utterance.utter()
    if utterance.is_last:
        return
    for previous in reversed(ANIMALS[:index]):
        if utterance.is_last:
            break
        utterance = UTTERANCES[previous]
        feature = f" that {utterance}" if isinstance(utterance, Feature) else ""
        yield f"She swallowed the {current} to catch the {previous}{feature}."
        current = previous
    else:
        yield utterance.utter()


def intersperse(sep, iterable):
    iterator = iter(iterable)
    yield next(iterator)
    for e in iterator:
        yield sep
        yield e


def recite(start_verse, end_verse):
    lines = chain(
        *intersperse(
            ("", ),
            (
                make_stanza(stanza - 1)
                for stanza in range(start_verse, end_verse + 1)
            )
        )
    )
    return [str(line) for line in lines]
