// let's start somewhere... will improve it later

#[derive(Debug, PartialEq, Eq)]
pub struct Dna(Vec<DnaNucleotide>);

#[derive(Debug, PartialEq, Eq)]
pub enum DnaNucleotide {
    A,
    C,
    G,
    T,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Rna(Vec<RnaNucleotide>);

#[derive(Debug, PartialEq, Eq)]
pub enum RnaNucleotide {
    A,
    C,
    G,
    U,
}

impl TryFrom<char> for RnaNucleotide {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'A' => Ok(RnaNucleotide::A),
            'C' => Ok(RnaNucleotide::C),
            'G' => Ok(RnaNucleotide::G),
            'U' => Ok(RnaNucleotide::U),
            _ => Err(format!("Cannot make RnaNucleotide from {value}")),
        }
    }
}

impl TryFrom<char> for DnaNucleotide {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'A' => Ok(DnaNucleotide::A),
            'C' => Ok(DnaNucleotide::C),
            'G' => Ok(DnaNucleotide::G),
            'T' => Ok(DnaNucleotide::T),
            _ => Err(format!("Cannot make DnaNucleotide from {value}")),
        }
    }
}

impl Dna {
    pub fn new(dna: &str) -> Result<Dna, usize> {
        dna.chars()
            .enumerate()
            .map(|(i, chr)| chr.try_into().map_err(|_| i))
            .collect()
    }

    pub fn into_rna(self) -> Rna {
        self.0
            .iter()
            .map(|nucleotide| match nucleotide {
                DnaNucleotide::C => RnaNucleotide::G,
                DnaNucleotide::G => RnaNucleotide::C,
                DnaNucleotide::T => RnaNucleotide::A,
                DnaNucleotide::A => RnaNucleotide::U,
            })
            .collect()
    }
}

impl Rna {
    pub fn new(rna: &str) -> Result<Rna, usize> {
        rna.chars()
            .enumerate()
            .map(|(i, chr)| chr.try_into().map_err(|_| i))
            .collect()
    }
}

impl FromIterator<RnaNucleotide> for Rna {
    fn from_iter<T: IntoIterator<Item = RnaNucleotide>>(iter: T) -> Self {
        Rna(iter.into_iter().collect())
    }
}

impl FromIterator<DnaNucleotide> for Dna {
    fn from_iter<T: IntoIterator<Item = DnaNucleotide>>(iter: T) -> Self {
        Dna(iter.into_iter().collect())
    }
}
