const TRANSCRIBED = {
    "C": "G",
    "G": "C",
    "T": "A",
    "A": "U"
};

export const toRna = (dna) => {
    return dna.split("").map((c) => TRANSCRIBED[c]).join("");
};
