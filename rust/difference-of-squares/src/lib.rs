// This looks pretty sad when implemented generically. :-(

extern crate num;
extern crate num_iter;

use num::*;
use std::ops::*;
use std::iter::*;

pub fn difference<T>(i: T) -> T
    where T: Copy + ToPrimitive + Add<Output = T> + PartialOrd + Clone + One + Sum + Sub<Output = T>
{
    T::sub(square_of_sum(i), sum_of_squares(i))
}

pub fn sum_of_squares<T>(i: T) -> T
    where T: ToPrimitive + Add<Output = T> + PartialOrd + Clone + One + Sum
{
    num_iter::range_inclusive(T::one(), i).map(|x| pow(x, 2)).sum()
}

pub fn square_of_sum<T>(i: T) -> T
    where T: ToPrimitive + Add<Output = T> + PartialOrd + Clone + One + Sum
{
    let sum = num_iter::range_inclusive(T::one(), i).sum();
    pow(sum, 2)
}
