pub fn square(s: u32) -> u64 {
    assert!(1 <= s && s <= 64, "Square must be between 1 and 64");
    let (v, _) = geometric(2, 1, s);
    v
}

pub fn total() -> u64 {
    let (_, sum) = geometric(2, 1, 64);
    sum
}

// Geometric progression. Keep and return sum as well to optimize for total()
// usecase.
pub fn geometric(ratio: u64, scale: u64, term: u32) -> (u64, u64) {

    let mut v = scale;
    let mut sum = scale;

    for _ in 1..term {
        v *= ratio;
        sum += v;
    }

    (v, sum)
}
