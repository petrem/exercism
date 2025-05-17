export function translate(rna) {
  return [...translateRnaIter(rna ?? [])];
}

function* translateRnaIter(rna) {
  for (const codon of groupBy(rna, 3)) {
    const aminoAcid = CODON_TO_AMINOACID[codon];
    switch (aminoAcid) {
      case "STOP":
        return;
      case undefined:
        throw new Error("Invalid codon");
    }
    yield aminoAcid;
  }
}

function* groupBy(xs, m) {
  for (const k of Array(Math.ceil(xs.length / m)).keys()) {
    yield xs.slice(k*m, (k+1)*m);
  }
}

const CODON_TO_AMINOACID = {
  AUG: "Methionine",
  UUU: "Phenylalanine",
  UUC: "Phenylalanine",
  UUA: "Leucine",
  UUG: "Leucine",
  UCU: "Serine",
  UCC: "Serine",
  UCA: "Serine",
  UCG: "Serine",
  UAU: "Tyrosine",
  UAC: "Tyrosine",
  UGU: "Cysteine",
  UGC: "Cysteine",
  UGG: "Tryptophan",
  UAA: "STOP",
  UAG: "STOP",
  UGA: "STOP",
}
