#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    SpanTooLong,
    InvalidDigit(char),
}

pub fn lsp(string_digits: &str, span: usize) -> Result<u64, Error> {
    match (string_digits.len(), span) {
        (0, 0) => Ok(1),
        (0, _) => Err(Error::SpanTooLong),
        (n, _) if n < span => Err(Error::SpanTooLong),
        _ => {
            let mut numbers: Vec<u64> = vec![];
            for chr in string_digits.chars() {
                let digit = chr
                    .is_ascii_digit()
                    .then_some(chr as u8 - b'0')
                    .ok_or(Error::InvalidDigit(chr))?;
                numbers.push(digit as u64);
            }
            Ok(numbers.windows(span).fold(0, |max_prod, window| {
                let prod = window.iter().product();
                if prod > max_prod {
                    prod
                } else {
                    max_prod
                }
            }))
        }
    }
}
