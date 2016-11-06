extern crate num;

use std::ops::*;
use num::*;


pub fn is_leap_year(year: u32) -> bool {
    divisible(year, 4) && (!divisible(year, 100) || divisible(year, 400))
}


fn divisible<T: Num + Rem>(n: T, divisor: T) -> bool {
    n % divisor == T::zero()
}
