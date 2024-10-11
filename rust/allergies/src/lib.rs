// Version 2: build a BTreeMap out of the scores

// Tried to use phf to statically map Allergen => bit flag
// but even if I impl'ed PhfHash for the enum, it still complained
// the key type is not supported.

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

use std::collections::BTreeSet;
use std::iter::zip;

use Allergen::*;
const ALLERGENS: [Allergen; 8] = [
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
];

pub struct Allergies(BTreeSet<Allergen>);

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self(BTreeSet::from_iter(
            zip(ALLERGENS, BitStream(score))
                .filter_map(|(allergen, is_present)| is_present.then_some(allergen)),
        ))
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.0.contains(allergen)
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        self.0.iter().cloned().collect()
    }
}

struct BitStream(u32);
impl Iterator for BitStream {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        let bit0 = (self.0 & 0x01) == 1;
        self.0 >>= 1;
        Some(bit0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bitstream_yields_bits_for_5() {
        let bitstream = BitStream(5);
        let bits: Vec<bool> = bitstream.take(8).collect();
        assert_eq!(
            bits,
            vec![true, false, true, false, false, false, false, false]
        );
    }
}
