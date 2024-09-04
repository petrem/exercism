mod internal {
    use std::fmt;

    enum Sound {
        Pling,
        Plang,
        Plong,
    }

    impl fmt::Display for Sound {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Pling => write!(f, "Pling"),
                Plang => write!(f, "Plang"),
                Plong => write!(f, "Plong"),
            }
        }
    }

    struct RainWord {
        modulus: u32,
        word: Sound,
    }

    impl RainWord {
        fn to_string_from(&self, number: u32) -> String {
            if number % self.modulus == 0 {
                self.word.to_string()
            } else {
                String::new()
            }
        }
    }

    use Sound::*;

    const RAIN_SPEAK: [RainWord; 3] = [
        RainWord {
            modulus: 3,
            word: Pling,
        },
        RainWord {
            modulus: 5,
            word: Plang,
        },
        RainWord {
            modulus: 7,
            word: Plong,
        },
    ];

    pub fn raindrops(n: u32) -> String {
        let word: String = RAIN_SPEAK.iter().map(|x| x.to_string_from(n)).collect();
        if word.is_empty() {
            n.to_string()
        } else {
            word
        }
    }
}

pub fn raindrops(n: u32) -> String {
    internal::raindrops(n)
}
