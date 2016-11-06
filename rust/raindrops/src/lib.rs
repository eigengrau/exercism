extern crate num;

use num::*;
use std::ops::*;


pub fn raindrops(num: u32) -> String {
    let mut sound =
        [raindrop(num, 3, "Pling"),
         raindrop(num, 5, "Plang"),
         raindrop(num, 7, "Plong")]
            .join("")
            .to_string();

    if sound.is_empty() {
        sound = num.to_string()
    }

    sound
}


fn raindrop(num: u32, divisor: u32, sound: &str) -> &str {
    if divisible(num, divisor) { sound } else { "" }
}


fn divisible<T: Num + Rem + Eq>(n: T, divisor: T) -> bool {
    n % divisor == T::zero()
}
