def recite(start_verse, end_verse):
    return [_verse(n) for n in range(start_verse, end_verse + 1)]


def _verse(n):
    things_that_did = " ".join(
        f"the {thing} that {did}" for thing, did in reversed(_THINGS_THAT_DID[:n])
    )
    return f"This is {things_that_did}."


_THINGS_THAT_DID = [
    ("house", "Jack built"),
    ("malt", "lay in"),
    ("rat", "ate"),
    ("cat", "killed"),
    ("dog", "worried"),
    ("cow with the crumpled horn", "tossed"),
    ("maiden all forlorn", "milked"),
    ("man all tattered and torn", "kissed"),
    ("priest all shaven and shorn", "married"),
    ("rooster", "crowed in the morn that woke"),
    ("farmer sowing his corn", "kept"),
    ("horse and the hound and the horn", "belonged to"),
]

# ("morn", "woke"), could be a thing that did...
