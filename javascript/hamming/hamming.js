//
// This is only a SKELETON file for the 'Hamming' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const compute = (dna1, dna2) => {
    if (dna1.length !== dna2.length) {
        throw new Error('strands must be of equal length');
    }

    return zipWith((a, b) => a === b, [[...dna1], [...dna2]])
        .filter((x) => !x)
        .length
    
};

const zipWith = (op, rs) => rs[0].map(
  (_, idx) => (rs.map((r) => r[idx])).reduce(op)
);
