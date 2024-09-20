mod internal {
    use std::fmt;

    #[derive(Clone, Copy)]
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
        fn sound(&self, number: u32) -> Option<Sound> {
            (number % self.modulus == 0).then_some(self.word)
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
        let rainwords = String::from_iter(
            RAIN_SPEAK
                .iter()
                .filter_map(|rw| rw.sound(n))
                .map(|rw| rw.to_string()),
        );
        rainwords
            .is_empty()
            .then(|| n.to_string())
            .unwrap_or(rainwords)
    }
}

pub fn raindrops(n: u32) -> String {
    internal::raindrops(n)
}
