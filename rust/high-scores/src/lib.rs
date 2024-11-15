#[derive(Debug)]
pub struct HighScores {
    scores: Vec<u32>,
    top: Box<dyn Top>,
}

impl HighScores {
    pub fn new(scores: &[u32]) -> Self {
        let top: Box<dyn Top> = scores.iter().fold(Box::new(Top0), |acc, s| acc.insert(*s));
        Self {
            scores: scores.to_vec(),
            top,
        }
    }

    pub fn scores(&self) -> &[u32] {
        self.scores.as_slice()
    }

    pub fn latest(&self) -> Option<u32> {
        self.scores.last().cloned()
    }

    pub fn personal_best(&self) -> Option<u32> {
        self.top.get().first().cloned()
    }

    pub fn personal_top_three(&self) -> Vec<u32> {
        self.top.get()
    }
}

trait Top: std::fmt::Debug {
    fn get(&self) -> Vec<u32>;
    fn insert(self: Box<Self>, value: u32) -> Box<dyn Top>;
}

#[derive(Debug)]
struct Top0;
#[derive(Debug)]
struct Top1(u32);
#[derive(Debug)]
struct Top2(u32, u32);
#[derive(Debug)]
struct Top3(u32, u32, u32);

impl Top for Top0 {
    fn get(&self) -> Vec<u32> {
        vec![]
    }

    fn insert(self: Box<Self>, value: u32) -> Box<dyn Top> {
        Box::new(Top1(value))
    }
}

impl Top for Top1 {
    fn get(&self) -> Vec<u32> {
        vec![self.0]
    }

    fn insert(self: Box<Self>, value: u32) -> Box<dyn Top> {
        if self.0 < value {
            Box::new(Top2(value, self.0))
        } else {
            Box::new(Top2(self.0, value))
        }
    }
}

impl Top for Top2 {
    fn get(&self) -> Vec<u32> {
        vec![self.0, self.1]
    }

    fn insert(self: Box<Self>, value: u32) -> Box<dyn Top> {
        if self.0 < value {
            Box::new(Top3(value, self.0, self.1))
        } else if self.1 < value {
            Box::new(Top3(self.0, value, self.1))
        } else {
            Box::new(Top3(self.0, self.1, value))
        }
    }
}

impl Top for Top3 {
    fn get(&self) -> Vec<u32> {
        vec![self.0, self.1, self.2]
    }

    fn insert(self: Box<Self>, value: u32) -> Box<dyn Top> {
        if value > self.2 {
            if value >= self.0 {
                Box::new(Top3(value, self.0, self.1))
            } else if value <= self.1 {
                Box::new(Top3(self.0, self.1, value))
            } else {
                Box::new(Top3(self.0, value, self.1))
            }
        } else {
            self
        }
    }
}
