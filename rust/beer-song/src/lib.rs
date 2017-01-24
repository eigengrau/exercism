pub fn verse(verse_num: u32) -> String {

    if verse_num > 0 {
        format!("{some_Bottles} of beer on the wall, {some_bottles} of beer.
Take {one} down and pass it around, {fewer_bottles} of beer on the wall.
",
                some_bottles = inflect("bottle", verse_num, false),
                some_Bottles = inflect("bottle", verse_num, true),
                fewer_bottles = inflect("bottle", verse_num - 1, false),
                one = if verse_num != 1 { "one" } else { "it" })

    } else {
        "No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.
"
            .to_string()
    }

}


pub fn sing(from: u32, to: u32) -> String {

    let mut song: String = String::new();

    // No negative step_by() yet.
    for verse_num in (to..from+1).rev() {
        song += &verse(verse_num);
        if verse_num != to {
            song += "\n";
        }
    }

    song

}


fn inflect(noun: &str, num: u32, capitalize: bool) -> String {

    if num == 0 {
        if capitalize {
            format!("No more {}s", noun)
        } else {
            format!("no more {}s", noun)
        }
    } else if num == 1 {
        format!("{} {}", num, noun)
    } else {
        format!("{} {}s", num, noun)
    }

}
