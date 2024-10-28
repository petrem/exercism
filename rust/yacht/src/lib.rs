/// Score computation and checking is more shambolic than in previous iteration, but
/// here I played with functions and closures. And macros, yey!

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

/*
fn constant<A: Clone + 'static, B>(a: A) -> Box<dyn Fn(B) -> A> {
    Box::new(move |_b| a.clone())
}
*/

/// Poor person's `const`: returns a closure ignoring its argument and returning the
/// pre-defined value.
#[macro_export]
macro_rules! constant {
    ( $x:literal ) => {
        Box::new(|_| $x)
    };
}

/// Bool type case analysis. Like a ternary operator.
macro_rules! bool {
    ( $test:expr, $on_false:expr, $on_true:expr ) => {
        if $test {
            $on_true
        } else {
            $on_false
        }
    };
}

impl Category {
    fn scoring(&self) -> Scoring {
        match self {
            Category::Ones => Scoring::new(constant!(true), Box::new(Self::score_singles(1))),
            Category::Twos => Scoring::new(constant!(true), Box::new(Self::score_singles(2))),
            Category::Threes => Scoring::new(constant!(true), Box::new(Self::score_singles(3))),
            Category::Fours => Scoring::new(constant!(true), Box::new(Self::score_singles(4))),
            Category::Fives => Scoring::new(constant!(true), Box::new(Self::score_singles(5))),
            Category::Sixes => Scoring::new(constant!(true), Box::new(Self::score_singles(6))),
            Category::FullHouse => {
                Scoring::new(Box::new(Self::is_full_house), Box::new(Self::score_total))
            }
            Category::Choice => Scoring::new(constant!(true), Box::new(Self::score_total)),
            Category::FourOfAKind => Scoring::new(
                Box::new(Self::is_four_of_a_kind),
                Box::new(Self::score_four_of_a_kind),
            ),
            Category::LittleStraight => Scoring::new(Box::new(Self::is_straight(1)), constant!(30)),
            Category::BigStraight => Scoring::new(Box::new(Self::is_straight(2)), constant!(30)),
            Category::Yacht => Scoring::new(Box::new(Self::is_yacht), constant!(50)),
        }
    }

    fn is_full_house(dice: &Dice) -> bool {
        let mut dice = *dice;
        dice.sort_unstable();
        dice[0] == dice[1]
            && dice[3] == dice[4]
            && (dice[2] == dice[1] || dice[2] == dice[3])
            && dice[0] != dice[4]
    }

    fn count_from(idx: usize, dice: &Dice) -> usize {
        assert!(idx < N_FACES);
        dice.iter().skip(idx).filter(|&&f| f == dice[idx]).count()
    }

    fn is_four_of_a_kind(dice: &Dice) -> bool {
        Self::count_from(0, dice) >= 4 || Self::count_from(1, dice) == 4
    }

    fn is_yacht(dice: &Dice) -> bool {
        Self::count_from(0, dice) == 5
    }

    fn is_straight(start: u8) -> CheckerFn {
        Box::new(move |&dice| {
            let mut dice = dice.to_vec();
            dice.sort_unstable();
            dice == (start..(start + 5)).collect::<Vec<_>>()
        })
    }

    fn score_singles(face: u8) -> ScorerFn {
        Box::new(move |dice| dice.iter().filter(|&&d| face == d).count() as u8 * face)
    }

    fn score_total(dice: &Dice) -> u8 {
        dice.iter().sum()
    }

    fn score_four_of_a_kind(dice: &Dice) -> u8 {
        // it's a shame we call `count_from` twice
        4 * bool!(Self::count_from(0, dice) >= 4, dice[1], dice[0])
    }
}

type CheckerFn = Box<dyn Fn(&Dice) -> bool>;
type ScorerFn = Box<dyn Fn(&Dice) -> u8>;

struct Scoring {
    checker: CheckerFn,
    scorer: ScorerFn,
}

impl Scoring {
    fn new(checker: CheckerFn, scorer: ScorerFn) -> Self {
        Self { checker, scorer }
    }
}

const N_FACES: usize = 5;
type Dice = [u8; N_FACES];

pub fn score(dice: Dice, category: Category) -> u8 {
    let Scoring { checker, scorer } = category.scoring();
    bool!(checker(&dice), 0, scorer(&dice))
}
