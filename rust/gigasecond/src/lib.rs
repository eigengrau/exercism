extern crate chrono;

use chrono::*;


pub fn after(date: DateTime<UTC>) -> DateTime<UTC> {
    // No exponential integer literals in Rust? :(
    date + Duration::seconds(1000000000)
}
