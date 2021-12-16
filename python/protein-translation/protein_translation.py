import re


CONDOS_RE_MAP = {
    re.compile("AUG"): "Methionine",
    re.compile("UU[UC]"): "Phenylalanine",
    re.compile("UU[AG]"): "Leucine",
    re.compile("UC[UCAG]"): "Serine",
    re.compile("UA[UC]"): "Tyrosine",
    re.compile("UG[UC]"): "Cysteine",
    re.compile("UGG"): "Tryptophan",
    re.compile("U(?:A[AG]|GA)"): "STOP",
}


def _match_codon(codon):
    for r, match in CONDOS_RE_MAP.items():
        if re.match(r, codon):
            return match
    return None


def _match_codons(strand):
    for i in range(0, len(strand) - 3 + 1, 3):
        match = _match_codon(strand[i:i+3])
        if match == "STOP":
            return
        yield match


def proteins(strand):
    return list(_match_codons(strand))
