const TRANSCRIBED = {
    "C": "G",
    "G": "C",
    "T": "A",
    "A": "U"
};

export const toRna = (dna) => [...dna].map((c) => TRANSCRIBED[c]).join("");
