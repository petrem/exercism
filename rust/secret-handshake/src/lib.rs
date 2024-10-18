pub fn actions(n: u8) -> Vec<&'static str> {
    let mut response = vec![];
    Action::bits_to_actions(n).for_each(|action| action.implement_on(&mut response));
    response
}

enum Action {
    Wink,
    DoubleBlink,
    CloseEyes,
    Jump,
    Reverse,
}

impl Action {
    fn implement_on(&self, actions: &mut Vec<&'static str>) {
        match self {
            Action::Wink => actions.push("wink"),
            Action::DoubleBlink => actions.push("double blink"),
            Action::CloseEyes => actions.push("close your eyes"),
            Action::Jump => actions.push("jump"),
            Action::Reverse => actions.reverse(),
        }
    }

    fn bits_to_actions(n: u8) -> impl Iterator<Item = Action> {
        std::iter::zip(ACTIONS_ORDER, BitStream(n))
            .filter_map(|(action, bit_set)| bit_set.then_some(action))
    }
}

const ACTIONS_ORDER: [Action; 5] = [
    Action::Wink,
    Action::DoubleBlink,
    Action::CloseEyes,
    Action::Jump,
    Action::Reverse,
];

struct BitStream(u8);
impl Iterator for BitStream {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        let bit0 = (self.0 & 0x01) == 1;
        self.0 >>= 1;
        Some(bit0)
    }
}
