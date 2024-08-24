pub fn is_leap_year(year: u64) -> bool {
    //TODO: are there partials to make an "is_year_divisible_by"?
    is_divisible_by(year, 4) && (!is_divisible_by(year, 100) || is_divisible_by(year, 400))
}

//TODO: have this work with integers in general
fn is_divisible_by(dividend: u64, divisor: u64) -> bool {
    dividend % divisor == 0
}
