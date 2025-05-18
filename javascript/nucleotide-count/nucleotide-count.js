export function countNucleotides(strand) {
    return Array.from(
        [...strand].reduce((counts, nucleotide) => {
            if (counts.has(nucleotide)) {
                return counts.set(nucleotide, counts.get(nucleotide) + 1);
            }
            throw new Error("Invalid nucleotide in strand")
        },
                           new Map([..."ACGT"].map((n) => [n, 0]))).values()).join(" ");
}

