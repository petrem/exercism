// Nucleotides

enum Nucleotide {A, C, G, T, U }

impl TryFrom<char> for Nucleotide {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'A' => Ok(Nucleotide::A),
            'C' => Ok(Nucleotide::C),
            'G' => Ok(Nucleotide::G),
            'T' => Ok(Nucleotide::T),
            'U' => Ok(Nucleotide::U),
            _ => Err(format!("Cannot make RnaNucleotide from {value}")),
        }
    }
}

trait Strand {
    fn read(strand: &str) -> Result<Vec<Nucleotide>, usize> {
        strand
            .chars()
            .enumerate()
            .map(|(i, chr)| chr
                 .try_into()
                 .map_err(|_| i)
                 .map(|&nucleotide| {
                     if Self::valid(nucleotide) { Ok(*nucleotide) } else {Err(i)}})
            ).collect()
    }
    fn valid(nucleotide: &Nucleotide) -> bool;
}

impl Strand for Rna {
    fn valid(nucleotide: &Nucleotide) -> bool {
        match nucleotide {
            Nucleotide::T => false,
            _ => true,
        }
    }
}

impl TryFrom<&str> for Rna {
    type Error = usize;
    
    fn try_from(rna: &str) -> Result<Self, Self::Error> {
        let strand = Strand::read(rna);
    }
}
