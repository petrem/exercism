// Obviously, the disadvantage of this approach is that checks may be run several times.

pub fn reply(message: &str) -> &str {
    RESPONSES
        .iter()
        .find(|msg| (msg.cond)(message))
        .unwrap_or(&Response::default())
        .response
}

struct Response {
    cond: fn(&str) -> bool,
    response: &'static str,
}

impl Default for Response {
    fn default() -> Self {
        Self {
            cond: |_| true,
            response: "Whatever.",
        }
    }
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

const RESPONSES: [Response; 4] = [
    Response {
        cond: is_silence,
        response: "Fine. Be that way!",
    },
    Response {
        cond: |phrase| is_shout(phrase) && is_question(phrase),
        response: "Calm down, I know what I'm doing!",
    },
    Response {
        cond: is_question,
        response: "Sure.",
    },
    Response {
        cond: is_shout,
        response: "Whoa, chill out!",
    },
];
