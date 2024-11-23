// This is slow -- likely it'll timeout.

use std::{collections::{HashMap, HashSet}, iter::once, ops::Range, str::FromStr};
use itertools::{self, Itertools};

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    // Parse the string into a Puzzle struct,
    // generate valid solutions (permutations of digits for each letters that match the requirements)
    // and pick the first.
    let puzzle: Puzzle = input.parse().ok()?;
    let solution = puzzle.generate_solutions().next()?;
    Some(HashMap::from_iter(puzzle.solve(&solution)))
}

/// A term, as ordered array of indexes into the list of symbols
#[derive(Debug, PartialEq, Eq)]
struct Term(Vec<usize>);

impl Term {
    fn value(&self, values: &[u64]) -> u64 {
        self.0.iter().map(|&l| values[l]).fold(0, |value, digit| value * 10 + digit)
    }

    /// Try to return a Term() from a string `s` and a list of symbols, `letters`.
    fn parse(s: &str, letters: &[char]) -> Result<Self, ()> {
        Ok(Term(s.chars().map(|c| letters.iter().position(|&l| l == c)).collect::<Option<Vec<usize>>>().ok_or(())?))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Puzzle {
    /// all the unique symbols; first are those that cannot be 0
    /// (being on the first position in a word)
    symbols: Vec<char>, //HashSet<char>,
    /// range of positions in `letters` where they are first letters
    first_letters: Range<usize>,
    terms: Vec<Term>,
    rhs: Term,
}

impl Puzzle {
    fn generate_solutions(&self) -> impl Iterator<Item=Vec<u64>> + '_ {
        // self.letters ordered such that
        (0u64..10)
            .permutations(self.symbols.len())
            .filter(|candidate| self.verify_solution(candidate))
    }

    fn verify_solution(&self, candidate: &[u64]) -> bool {
        (!candidate[self.first_letters.clone()].contains(&0)) && (self.terms.iter().map(|term| term.value(candidate)).sum::<u64>() == self.rhs.value(candidate))
    }

    /// Map symbols to values.
    // Rather than returning a HashMap directly, return an iterator
    // so that the caller may choose what they want to with the result.
    fn solve<'a>(&self, solution: &'a [u64]) -> impl Iterator<Item=(char, u8)> + 'a {
        std::iter::zip(self.symbols.clone().into_iter(), solution.into_iter().map(|d| *d as u8))
    }
}

impl FromStr for Puzzle {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        if let [lhs, rhs] = input.split("==").collect::<Vec<_>>()[..] {
            let lhs: Vec<_> = lhs.split('+').map(|w| w.trim()).collect();
            let rhs = rhs.trim();
            let mut letters: HashSet<char> = HashSet::new();
            let mut firsts: HashSet<char> = HashSet::new();

            for word in lhs.iter().chain(once(&rhs)) {
                firsts.insert(word.chars().next().ok_or(())?);
                letters.extend(word.chars());
            }

            let first_letters = 0..firsts.len();
            let letters: Vec<char> = letters.difference(&firsts).map(|c| *c).collect();
            let symbols: Vec<char> = firsts.into_iter().chain(letters.into_iter()).collect();
            let terms: Vec<Term> = lhs.iter().map(|term| Term::parse(term, &symbols)).collect::<Result<_, ()>>()?;
            let rhs = Term::parse(rhs, &symbols)?;
            Ok(Self { symbols, first_letters, terms, rhs } )
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Coverage is poor -- I just used these to shape and quickly check some internals

    #[test]
    fn parses_puzzle() {
        let input = "ab + bc == de";

        let first_letters = 0..3;

        let puzzle: Puzzle = input.parse().unwrap();
        assert_eq!(puzzle.first_letters, first_letters);

        let first_symbols: Vec<_> = puzzle.symbols[first_letters.clone()].iter().cloned().sorted().collect();
        let expected_first_symbols = ['a','b','d'];
        assert_eq!(first_symbols, expected_first_symbols);

        let rhs: Vec<_> = puzzle.rhs.0.iter().map(|&i| puzzle.symbols.iter().cloned().nth(i).unwrap()).collect();
        let expected_rhs = ['d', 'e'];
        assert_eq!(rhs, expected_rhs);

        let terms: Vec<Vec<char>> = puzzle.terms.iter().map(|term| term.0.iter().map(|&i| puzzle.symbols.iter().cloned().nth(i).unwrap()).collect()).collect();
        let expected_terms = [['a', 'b'], ['b', 'c']];
        assert_eq!(terms, expected_terms);
    }

    #[test]
    fn term_value() {
        let term = Term(vec![1, 2, 4]);

        assert_eq!(term.value(&[0, 1, 2, 3, 8]), 128);
    }

    #[test]
    fn verify_valid_solution() {
        // ab + bc = de --> 21 + 19 = 40 (a: 2, b: 1, d: 4, c: 9, e: 0)
        let symbols = vec!['a','b','d', 'c', 'e'];
        let first_letters = 0..3;
        let terms = vec![Term(vec![0, 1]), Term(vec![1, 3])];
        let rhs = Term(vec![2, 4]);
        let puzzle = Puzzle { symbols, first_letters, terms, rhs };
        let candidate = vec![2, 1, 4, 9, 0];

        assert!(puzzle.verify_solution(&candidate));
    }

    #[test]
    fn verify_invalid_solution_term_starts_with_zero() {
        // ab + bc = de --> 08 + 83 = 91 (a: 0, b: 8, d: 9, c: 3, e: 1)
        let symbols = vec!['a','b','d', 'c', 'e'];
        let first_letters = 0..3;
        let terms = vec![Term(vec![0, 1]), Term(vec![1, 3])];
        let rhs = Term(vec![2, 4]);
        let puzzle = Puzzle { symbols, first_letters, terms, rhs };
        let candidate = vec![0, 8, 9, 3, 1];

        assert!(!puzzle.verify_solution(&candidate));
    }

    #[test]
    fn verify_invalid_solution_terms_sum_not_equal_rhs() {
        // ab + bc = de --> 31 + 19 != 40 (a: 3, b: 1, d: 4, e: 0)
        let symbols = vec!['a','b','d', 'c', 'e'];
        let first_letters = 0..3;
        let terms = vec![Term(vec![0, 1]), Term(vec![1, 3])];
        let rhs = Term(vec![2, 4]);
        let puzzle = Puzzle { symbols, first_letters, terms, rhs };
        let candidate = vec![3, 1, 4, 9, 0];

        assert!(!puzzle.verify_solution(&candidate));
    }

    #[test]
    fn generated_solutions_include_known_valid_solution() {
        // ab + bc = de --> 21 + 19 = 40 (a: 2, b: 1, d: 4, e: 0)
        let symbols = vec!['a','b','d', 'c', 'e'];
        let first_letters = 0..3;
        let terms = vec![Term(vec![0, 1]), Term(vec![1, 3])];
        let rhs = Term(vec![2, 4]);
        let puzzle = Puzzle { symbols, first_letters, terms, rhs };
        let candidate = vec![2, 1, 4, 9, 0];
        let solutions: Vec<_> = puzzle.generate_solutions().collect();

        assert!(solutions.contains(&candidate));
    }
}
