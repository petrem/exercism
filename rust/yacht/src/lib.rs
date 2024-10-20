// we have to start somewhere...

#[derive(Debug)]
pub enum Category {
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}

type Face = u8;
type Score = u8;
type Dice = [Face; 5];

struct Throw {
    counts: Vec<u8>,
}

impl Throw {
    fn new(dice: Dice) -> Self {
        let mut counts = vec![0 /*filler*/, 0, 0, 0, 0, 0, 0];

        for face in dice {
            assert!((1..=6).contains(&face)); // TODO: do we do this in Rust?
            counts[face as usize] += 1;
        }
        Self { counts }
    }

    fn singles(&self, face: u8) -> Score {
        self.counts[face as usize] * face
    }

    fn full_house(&self) -> Score {
        let mut ordered_face_counts: Vec<u8> =
            self.counts.iter().filter(|x| **x != 0).copied().collect();
        ordered_face_counts.sort();
        if ordered_face_counts == vec![2, 3] {
            self.total()
        } else {
            0
        }
    }

    fn four_of_a_kind(&self) -> Score {
        self.face_with_top_count()
            .map_or(0, |(face, count)| if count >= 4 { face * 4 } else { 0 })
    }

    fn straight(&self, start: Face) -> Score {
        if self
            .counts
            .split_at(start as usize)
            .1
            .iter()
            .take(5)
            .all(|c| *c == 1)
        {
            30
        } else {
            0
        }
    }

    fn choice(&self) -> Score {
        self.total()
    }

    fn yacht(&self) -> Score {
        self.face_with_top_count()
            .map_or(0, |(_, c)| if c == 5 { 50 } else { 0 })
    }

    fn total(&self) -> Score {
        self.counts
            .iter()
            .enumerate()
            .skip(1)
            .fold(0, |acc, (i, c)| acc + (i as u8) * c)
    }

    fn face_with_top_count(&self) -> Option<(Face, u8)> {
        self.counts
            .iter()
            .enumerate()
            .max_by_key(|(_, &x)| x)
            .map(|(top_face, &count)| (top_face as Face, count))
    }
}

pub fn score(dice: Dice, category: Category) -> Score {
    let throw = Throw::new(dice);
    match category {
        Category::Ones => throw.singles(1),
        Category::Twos => throw.singles(2),
        Category::Threes => throw.singles(3),
        Category::Fours => throw.singles(4),
        Category::Fives => throw.singles(5),
        Category::Sixes => throw.singles(6),
        Category::FullHouse => throw.full_house(),
        Category::FourOfAKind => throw.four_of_a_kind(),
        Category::LittleStraight => throw.straight(1),
        Category::BigStraight => throw.straight(2),
        Category::Choice => throw.choice(),
        Category::Yacht => throw.yacht(),
    }
}
