// Obviously, the disadvantage of this approach is that checks may be run when not needed.

pub fn reply(message: &str) -> &str {
    Response::from(message).0
}

fn is_silence(phrase: &str) -> bool {
    phrase.chars().all(|c| c.is_whitespace())
}

fn is_question(phrase: &str) -> bool {
    phrase.trim_end().ends_with('?')
}

fn is_shout(phrase: &str) -> bool {
    phrase.contains(|c: char| c.is_uppercase()) && !phrase.contains(|c: char| c.is_lowercase())
}

struct Response(&'static str);

impl From<&str> for Response {
    fn from(msg: &str) -> Self {
        match (is_silence(msg), is_question(msg), is_shout(msg)) {
            (true, _, _) => Response("Fine. Be that way!"),
            (false, true, true) => Response("Calm down, I know what I'm doing!"),
            (false, false, true) => Response("Whoa, chill out!"),
            (false, true, false) => Response("Sure."),
            _ => Response("Whatever."),
        }
    }
}
