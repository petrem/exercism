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
type CategoryFn = fn(Dice) -> Score;

struct FaceCounter {
    face: Face,
    count: u8,
}

struct Throw {
    dice: Dice,
    counts: Vec<FaceCount>,
}

impl Throw {
    fn new(dice: Dice) -> Self {
        Self {
            dice,
            counts: vec![],
        }
    }

           
    fn singles(face: u8, dice: Dice) -> u8 {
        0
    }
}

pub fn score(dice: Dice, category: CategoryFn) -> u8 {
    0
}
