use std::cmp::min;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

/// A struct to hold the results.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number of liters, including
    /// the first fill.
    pub moves: u8,
    /// Which bucket should end up with the desired number of liters? (Either "one" or "two")
    pub goal_bucket: Bucket,
    /// How many liters are left in the other bucket?
    pub other_bucket: u8,
}

/// Solve the bucket problem
pub fn solve(
    capacity_1: u8,
    capacity_2: u8,
    goal: u8,
    start_bucket: &Bucket,
) -> Option<BucketStats> {
    if goal > capacity_1 && goal > capacity_2 {
        return None;
    }
    let solution_maybe = match start_bucket {
        Bucket::One => Solver::new(BucketPlus::One(capacity_1), BucketPlus::Two(capacity_2)),
        Bucket::Two => Solver::new(BucketPlus::Two(capacity_2), BucketPlus::One(capacity_1)),
    }
    .solve(goal);
    solution_maybe.map(|path: Path| {
        let (goal_bucket, other_bucket) = if path.end_state.volume_1 == goal {
            (Bucket::One, path.end_state.volume_2)
        } else {
            (Bucket::Two, path.end_state.volume_1)
        };

        BucketStats {
            moves: path.steps,
            goal_bucket,
            other_bucket,
        }
    })
}

/// Buckets with their capacity
#[derive(Clone, Debug)]
enum BucketPlus {
    One(u8),
    Two(u8),
}

impl From<BucketPlus> for Bucket {
    fn from(value: BucketPlus) -> Self {
        match value {
            BucketPlus::One(_) => Bucket::One,
            BucketPlus::Two(_) => Bucket::Two,
        }
    }
}

/// State in the problem space: liquid content in each bucket.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct State {
    volume_1: u8,
    volume_2: u8,
}

impl State {
    fn new(volume_1: u8, volume_2: u8) -> Self {
        Self { volume_1, volume_2 }
    }
}

impl Default for State {
    fn default() -> Self {
        State::new(0, 0)
    }
}

/// Possible actions, leading to changes in the problem space.
#[derive(Clone, Debug)]
enum Action<'a> {
    Empty(&'a BucketPlus),
    Fill(&'a BucketPlus),
    Pour(&'a BucketPlus, &'a BucketPlus),
}

impl<'a> Action<'a> {
    fn perform(&self, state: &State) -> State {
        // TODO: deduplicate this code
        match self {
            Action::Empty(BucketPlus::One(_)) => State::new(0, state.volume_2),
            Action::Empty(BucketPlus::Two(_)) => State::new(state.volume_1, 0),
            Action::Fill(BucketPlus::One(capacity)) => State::new(*capacity, state.volume_2),
            Action::Fill(BucketPlus::Two(capacity)) => State::new(state.volume_1, *capacity),
            Action::Pour(BucketPlus::One(_), BucketPlus::Two(capacity)) => {
                let amount = min(state.volume_1, capacity - state.volume_2);
                State::new(state.volume_1 - amount, state.volume_2 + amount)
            }
            Action::Pour(BucketPlus::Two(_), BucketPlus::One(capacity)) => {
                let amount = min(state.volume_2, capacity - state.volume_1);
                State::new(state.volume_1 + amount, state.volume_2 - amount)
            }
            _ => panic!("impudent action"),
        }
    }
}

/// List of actions together with a resulting state.
/// Allows extending with an action, resulting in a new path with updated state.
#[derive(Debug)]
struct Path {
    steps: u8,
    end_state: State,
}

impl Path {
    fn extend(&self, action: &Action) -> Self {
        Self {
            steps: self.steps + 1,
            end_state: action.perform(&self.end_state),
        }
    }

    fn has_goal(&self, goal: u8) -> bool {
        self.end_state.volume_1 == goal || self.end_state.volume_2 == goal
    }
}

#[derive(Debug)]
struct Frontier {
    paths: Vec<Path>,
    seen: HashSet<State>,
}

impl Frontier {
    fn next(&self, actions: &[Action]) -> Option<Self> {
        let paths: Vec<Path> = self
            .paths
            .iter()
            .flat_map(|path| {
                actions
                    .iter()
                    .map(|action| path.extend(action))
                    .filter(|new_path| !self.seen.contains(&new_path.end_state))
            })
            .collect();
        if paths.is_empty() {
            None
        } else {
            let mut seen: HashSet<State> = self.seen.clone();
            seen.extend(paths.iter().map(|path| path.end_state));
            Some(Self { paths, seen })
        }
    }
}

struct Solver {
    bucket_1: BucketPlus,
    bucket_2: BucketPlus,
}

impl Solver {
    fn new(bucket_1: BucketPlus, bucket_2: BucketPlus) -> Self {
        Self { bucket_1, bucket_2 }
    }

    fn get_actions(&self) -> Vec<Action> {
        vec![
            Action::Empty(&self.bucket_1),
            Action::Empty(&self.bucket_2),
            Action::Fill(&self.bucket_1),
            Action::Fill(&self.bucket_2),
            Action::Pour(&self.bucket_1, &self.bucket_2),
            Action::Pour(&self.bucket_2, &self.bucket_1),
        ]
    }

    fn solve(&self, goal: u8) -> Option<Path> {
        let empty_state = State::new(0, 0);
        let first_step = Path {
            steps: 1,
            end_state: Action::Fill(&self.bucket_1).perform(&empty_state),
        };
        let first_state = first_step.end_state;
        let invalid_state = Action::Fill(&self.bucket_2).perform(&empty_state);
        let seed = Frontier {
            paths: vec![first_step],
            seen: HashSet::from([empty_state, first_state, invalid_state]),
        };
        let all_actions = self.get_actions();
        std::iter::successors(Some(dbg!(seed)), |frontier| frontier.next(&all_actions))
            .map(|frontier| frontier.paths.into_iter().find(|path| path.has_goal(goal)))
            .find(Option::is_some)
            .flatten()
    }
}
