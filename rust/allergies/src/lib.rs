// Version 1: a first attempt
// I don't like how I listed the allergens three times.

#[derive(Debug, PartialEq, Eq)]
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

use Allergen::*;

struct AllergenFlag(Allergen, u32);

impl AllergenFlag {
    fn test(&self, number: u32) -> bool {
        
    }
}

const ALLERGENS: [AllergenFlag; 8] = [
    AllergenFlag(Eggs, 0x01),
    AllergenFlag(Peanuts, 0x02),
    AllergenFlag(Shellfish, 0x04),
    AllergenFlag(Strawberries, 0x08),
    AllergenFlag(Tomatoes, 0x10),
    AllergenFlag(Chocolate, 0x20),
    AllergenFlag(Pollen, 0x40),
    AllergenFlag(Cats, 0x80),
];

pub struct Allergies {
    score: u32,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self { score }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.score & allergen_bit(allergen) != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        let mut result: Vec<Allergen> = Vec::new();
        for allergen in ALLERGENS {
            if self.is_allergic_to(&allergen) {
                result.push(allergen);
            }
        }
        result
    }
}
