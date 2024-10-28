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

fn constant<'a, A: Clone + 'static, B>(a: A) -> Box<dyn Fn(B) -> A> {
    Box::new(move |_b| a.clone())
}

impl Category {
    fn scoring(&self) -> Scoring {
        //let f = Box::new(constant::<bool, u32>(true.clone()));
        //let g = Box::new(constant::<u32, String>(12.clone()));
        
        match self {
            Category::Ones => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(1))),
            Category::Twos => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(2))),
            Category::Threes => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(3))),
            Category::Fours => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(4))),
            Category::Fives => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(5))),
            Category::Sixes => Scoring::new(Box::new(|_| true), Box::new(Self::score_singles(6))),
            Category::FullHouse => Scoring::new(Box::new(Self::is_full_house), Box::new(Self::score_total)),
            // Category::Choice => throw.total(),
            _ => Scoring::new(Box::new(|_| true), Box::new(|_| 255)),
        }
    }

    fn is_full_house(dice: &Dice) -> bool {
        let mut dice = dice.clone();
        dice.sort();
        dice[0] == dice[1] && dice[3] == dice[4] && (dice[2] == dice[1] || dice[2] == dice[3])
    }

    fn is_yacht_like(dice: &Dice) -> bool {
        false
    }

    fn score_singles(face: u8) -> ScorerFn {
        Box::new(move |dice| dice.iter().filter(|&&d| face == d).count() as u8 * face)
    }

    fn score_total(dice: &Dice) -> u8  {
        dice.iter().sum()
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
        Self {checker, scorer}
    }
}

type Dice = [u8; 5];

pub fn score(dice: Dice, category: Category) -> u8 {
    let Scoring { checker, scorer } = category.scoring();
    checker(&dice).then_some(scorer(&dice)).unwrap_or(0) // TODO: is this the idiomatic way? should I rather use and if?
}
