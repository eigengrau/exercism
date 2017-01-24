extern crate num;

use num::*;
use std::ops::Rem;

pub fn sum_of_multiples(limit: u32, factors: &Vec<u32>) -> u32 {
    let mut multiples = Vec::new();
    for n in 1..limit {
        let divides_n = |x: &u32| divisible(n, *x);
        if factors.iter().any(divides_n) {
            multiples.push(n)
        }
    }
    multiples.iter().sum()
}

fn divisible<T: Num + Rem + Eq>(n: T, divisor: T) -> bool {
    n % divisor == T::zero()
}
