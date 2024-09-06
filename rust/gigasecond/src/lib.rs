use time::{Duration, PrimitiveDateTime as DateTime, Time};

const GIGASECOND: Duration = Duration::seconds(1_000_000_000);
const GIGASECOND_WHOLE_DAYS: Duration = Duration::days(GIGASECOND.whole_days());
const GIGASECOND_LEFTOVER_SECONDS: Duration = GIGASECOND.saturating_sub(GIGASECOND_WHOLE_DAYS);

// Returns a DateTime one billion seconds after start.
pub fn after(start: DateTime) -> DateTime {
    // Let's assume we couldn't `start.checked_add(GIGASECOND).unwrap()`
    // and let's play with the various methods of the structs from the 'time' crate.
    let (date, time) = (start.date(), start.time());
    let (hour, minute, second) = time.as_hms();
    let time_as_duration =
        Duration::seconds(hour as i64 * 3600 + minute as i64 * 60 + second as i64);
    let overflow = time_as_duration + GIGASECOND_LEFTOVER_SECONDS;
    let overflow_days = Duration::days(overflow.whole_days());
    let overflow_secs = overflow - overflow_days;
    let d_hours = Duration::hours(overflow_secs.whole_hours());
    let d_minutes = Duration::minutes((overflow_secs - d_hours).whole_minutes());
    let d_seconds = overflow_secs - d_hours - d_minutes;
    DateTime::new(
        date + GIGASECOND_WHOLE_DAYS + overflow_days,
        Time::from_hms(
            d_hours.whole_hours() as u8,
            d_minutes.whole_minutes() as u8,
            d_seconds.whole_seconds() as u8,
        )
        .expect("what could've done this?!"),
    )
}
