use clock::*;

//
// Test Ordering
//

#[test]
fn clocks_with_same_time() {
    assert!(Clock::new(15, 37).cmp(&Clock::new(15, 37)).is_eq());
}

#[test]
fn clocks_a_minute_apart() {
    assert!(Clock::new(15, 36) < Clock::new(15, 37));
}

#[test]
fn clocks_an_hour_apart() {
    assert!(Clock::new(14, 37) < Clock::new(15, 37));
}

#[test]
fn clocks_with_hour_overflow() {
    assert!(Clock::new(10, 37).cmp(&Clock::new(34, 37)).is_eq());
}

#[test]
fn clocks_with_hour_overflow_by_several_days() {
    assert!(Clock::new(3, 11).cmp(&Clock::new(99, 11)).is_eq());
}

#[test]
fn clocks_with_negative_hour() {
    assert!(Clock::new(22, 40).cmp(&Clock::new(-2, 40)).is_eq());
}

#[test]
fn clocks_with_negative_hour_that_wraps() {
    assert_eq!(Clock::new(17, 3), Clock::new(-31, 3));
}
