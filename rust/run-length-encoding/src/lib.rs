pub fn encode(source: &str) -> String {
    group(source)
        .iter()
        .map(|cluster| match (cluster.len(), cluster.chars().next()) {
            (1, Some(c)) => format!("{c}"),
            (n, Some(c)) if n > 1 => format!("{n}{c}"),
            _ => panic!("Bug in `group()`, it returned group with empty string"),
        })
        .collect()
}

pub fn decode(source: &str) -> String {
    let tokenizer = Tokenize::new(source);
    let mut times: usize = 1;
    let mut result = String::new();
    for token in tokenizer {
        match token {
            Token::Number(n) => {
                times = n;
            }
            Token::Char(c) => {
                result.push_str(c.repeat(times).as_ref());
                times = 1;
            }
        }
    }
    result
}

/*
trait Span {
    fn span(&mut self) -> (Box<Self>, Box<Self>);
}

impl<T: ?Sized, U> Span for T
where T: Iterator<Item=U> + Default + FromIterator<U>,
      U: Clone + PartialEq {
    fn span(&mut self) -> (Box<T>, Box<T>) {
        if let Some(first) = self.next() {
            (Box::new(once(first.clone()).chain(self.take_while(|x| *x == first)).collect()), Box::new(self.collect()))
        } else {
            (Box::new(Default::default()), Box::new(Default::default()))
        }
    }
}
 */

fn span<P>(f: P, s: &str) -> (&str, &str)
where
    P: Fn(char) -> bool,
{
    let split_at = s.find(|c| !f(c)).unwrap_or(s.len());
    (&s[..split_at], &s[split_at..])
}

fn group(s: &str) -> Vec<&str> {
    let mut result = vec![];
    let mut s = s;
    while let Some(first) = s.chars().next() {
        let (left, right) = span(|c| c == first, s);
        result.push(left);
        s = right;
    }
    result
}

enum Token {
    Number(usize),
    Char(String),
}

struct Tokenize<'a> {
    source: &'a str,
}

impl<'a> Tokenize<'a> {
    fn new(source: &'a str) -> Self {
        Tokenize { source }
    }
}

impl<'a> Iterator for Tokenize<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.source.is_empty() {
            return None;
        }
        let (digits, rest) = span(|c| c.is_ascii_digit(), self.source);
        match digits.parse::<usize>() {
            Ok(num) => {
                self.source = rest;
                Some(Token::Number(num))
            }
            _ => {
                //TODO: what is a good way to split a str at unicode codepoints?
                let first: String = self.source.chars().take(1).collect();
                //let (c, rest) = self.source.split_at(1);
                //self.source = rest;
                self.source = &self.source[first.len()..];
                Some(Token::Char(first))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn span_empty_vec_yields_empty_strings() {
        let input: Vec<i32> = vec![];
        let expected: (Box<Vec<i32>>, Box<Vec<i32>>) = (Box::new(vec![]), Box::new(vec![]));
        //assert_eq!(input.span(), expected);
    }

    #[test]
    fn span_function_empty_string() {
        let input = "";
        let expected = ("", "");
        assert_eq!(span(|c| c == 'a', input), expected);
    }

    #[test]
    fn span_function_no_match() {
        let input = "aaabbb";
        let expected = ("", "aaabbb");
        assert_eq!(span(|c| c == 'c', input), expected);
    }

    #[test]
    fn span_function_matching() {
        let input = "aaabbb";
        let expected = ("aaa", "bbb");
        assert_eq!(span(|c| c == 'a', input), expected);
    }

    #[test]
    fn span_function_splits_one_matching_char() {
        let input = "a";
        let expected = ("a", "");
        assert_eq!(span(|c| c.is_lowercase(), input), expected);
    }

    #[test]
    fn span_function_splits_one_non_matching_char() {
        let input = "a";
        let expected = ("", "a");
        assert_eq!(span(|c| c.is_uppercase(), input), expected);
    }

    #[test]
    fn group_empty_string() {
        let input = "";
        let expected: Vec<&str> = vec![];
        assert_eq!(group(input), expected);
    }

    #[test]
    fn group_string_no_dups() {
        let input = "abc";
        let expected: Vec<&str> = vec!["a", "b", "c"];
        assert_eq!(group(input), expected);
    }
}
