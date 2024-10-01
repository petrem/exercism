pub fn reply(message: &str) -> &str {
    Response::from(message).0
}

struct Response(&'static str);

impl From<&str> for Response {
    fn from(msg: &str) -> Self {
        let msg = msg.trim_end();
        if msg.is_empty() {
            return Response("Fine. Be that way!");
        }
        let is_question = msg.ends_with('?');
        let is_shout =
            msg.contains(|c: char| c.is_uppercase()) && !msg.contains(|c: char| c.is_lowercase());
        match (is_question, is_shout) {
            (true, true) => Response("Calm down, I know what I'm doing!"),
            (false, true) => Response("Whoa, chill out!"),
            (true, false) => Response("Sure."),
            _ => Response("Whatever."),
        }
    }
}
