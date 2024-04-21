import re

PROTEIN = (
    r"(?P<Methionine>AUG)"
    r"|(?P<Phenylalanine>UU[UC])"
    r"|(?P<Leucine>UU[AG])"
    r"|(?P<Serine>UC[UCAG])"
    r"|(?P<Tyrosine>UA[UC])"
    r"|(?P<Cysteine>UG[UC])"
    r"|(?P<Tryptophan>UGG)"
)
STOP = r"U(?:A[AG]|GA)"
CODONS_RE = re.compile(
    rf"(?:{PROTEIN})(?:(?={PROTEIN})|$)"
)


def _protein(matchobj):
    """Get the name of the first group with a match."""
    
    return next(k for k, v in matchobj.groupdict().items() if v)


def proteins(strand):
    # Each match should have a single group.
    # This solution assumes no errors, as the tests in the track imply.
    #return list(_match_codons(strand))
    return [_protein(m) for m in CODONS_RE.finditer(strand)]
    # for m in CODONS_RE.finditer(strand):
    #     print(m.groupdict(), _protein(m))


if __name__ == "__main__":
    import sys
    proteins(sys.argv[1])
