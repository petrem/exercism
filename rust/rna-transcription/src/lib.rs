// Nucleotides

#[derive(Clone, Debug, PartialEq, Eq)]
enum Nucleotide {
    A,
    C,
    G,
    T,
    U,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Dna(Vec<Nucleotide>);

#[derive(Debug, PartialEq, Eq)]
pub struct Rna(Vec<Nucleotide>);

impl Dna {
    pub fn new(strand: &str) -> Result<Self, usize> {
        <Self as Strand>::from_str(strand)
            .collect::<Result<Vec<Nucleotide>, _>>()
            .map(Dna)
    }

    pub fn into_rna(&self) -> Rna {
        Rna::from_iter(self.0.iter().map(Self::translate))
    }

    fn translate(nucleotide: &Nucleotide) -> Nucleotide {
        match nucleotide {
            Nucleotide::A => Nucleotide::U,
            Nucleotide::C => Nucleotide::G,
            Nucleotide::G => Nucleotide::C,
            Nucleotide::T => Nucleotide::A,
            _ => panic!("Cannot translate U"),
        }
    }
}

impl Rna {
    pub fn new(strand: &str) -> Result<Self, usize> {
        <Self as Strand>::from_str(strand)
            .collect::<Result<Vec<Nucleotide>, _>>()
            .map(Rna)
    }
}

impl FromIterator<Nucleotide> for Rna {
    fn from_iter<I: IntoIterator<Item = Nucleotide>>(iter: I) -> Rna {
        Rna(iter.into_iter().collect())
    }
}

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
    const VALID: [Nucleotide; 4];
    fn valid(nucleotide: &Nucleotide) -> bool
    where
        Self: Sized,
    {
        <Self as Strand>::VALID.contains(nucleotide)
    }

    fn from_str(strand: &str) -> Box<dyn Iterator<Item = Result<Nucleotide, usize>> + '_>
    where
        Self: Sized,
    {
        Box::new(strand.chars().enumerate().map(|(i, chr)| {
            chr.try_into()
                .map_err(|_| i)
                .and_then(|nucleotide: Nucleotide| {
                    if Self::valid(&nucleotide) {
                        Ok(nucleotide)
                    } else {
                        Err(i)
                    }
                })
        }))
    }
}

impl Strand for Rna {
    const VALID: [Nucleotide; 4] = [Nucleotide::A, Nucleotide::C, Nucleotide::G, Nucleotide::U];
}

impl Strand for Dna {
    const VALID: [Nucleotide; 4] = [Nucleotide::A, Nucleotide::C, Nucleotide::G, Nucleotide::T];
}
