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

// fn constant1<'a, A, B>(a: &'a A) -> Box<dyn FnOnce(B) -> &'a A> {
//     Box::new(|_b| a)
// }

fn constant<A, B>(a: &A) -> Box<dyn FnOnce(B) -> A>
where A: Clone {
    Box::new(|_b| a.clone())
}

impl Category {
    fn scoring(&self) -> Scoring {
        match self {
            Category::Ones => Scoring::new(constant(&true), Self::score_singles(1)),
            // Category::Twos => throw.score_singles(2),
            // Category::Threes => throw.score_singles(3),
            // Category::Fours => throw.score_singles(4),
            // Category::Fives => throw.score_singles(5),
            // Category::Sixes => throw.score_singles(6),
            // Category::FullHouse => throw.full_house(),
            // Category::Choice => throw.total(),
            _ => Scoring::new(constant(&true), constant(&255)),
        }
    }

    fn is_full_house(dice: &mut Dice) -> bool {
        dice.sort();
        dice[0] == dice[1] && dice[3] == dice[4] && (dice[2] == dice[1] || dice[2] == dice[3])
    }

    fn is_yacht_like(dice: &mut Dice) -> bool {
        false
    }

    fn score_singles(face: u8) -> ScorerFn {
        Box::new(|dice| dice.iter().filter(|&&d| face == d).count() as u8 * face)
    }
}

type CheckerFn = Box<dyn FnOnce(&Dice) -> bool>;
type ScorerFn = Box<dyn FnOnce(&Dice) -> u8>;
    
struct Scoring {
    checker: CheckerFn,
    scorer: ScorerFn,
}

impl Scoring {
    fn new(checker: CheckerFn, scorer: ScorerFn) -> Self {
        Self {checker, scorer}
    }
}

type Dice = [u8; 5];

/*
struct Throw<'a> {
    dice: &'a Dice,
    counts: Vec<(u8, u8)>,
}

impl<'a> Throw<'a> {
    fn new(dice: &'a Dice) -> Self {
        Self {
            dice,
            counts: vec![],
        }
    }
           
    fn score_singles(&self, face: u8) -> u8 {
        self.dice.iter().filter(|&&d| face == d).count() as u8 * face
    }

    fn total(&self) -> u8 {
        self.dice.iter().sum()
    }

    fn is_full_house(&self) -> u8 {
        0
    }
}
*/

pub fn score(dice: Dice, category: Category) -> u8 {
    let Scoring { checker, scorer } = category.scoring();
    checker(&dice).then_some(scorer(&dice)).unwrap_or(0) // TODO: is this the idiomatic way? should I rather use and if?
}
