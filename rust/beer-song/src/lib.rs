pub fn verse(verse_num: u32) -> String {

    if verse_num > 0 {
        let one = if verse_num != 1 { "one" } else { "it" };
        format!("{before_inf_c} of beer on the wall, {before_inf} of beer.
Take {one} down and pass it around, {after_inf} of beer on the wall.
",
                before_inf=inflect("bottle", verse_num, false),
                before_inf_c=inflect("bottle", verse_num, true),
                after_inf=inflect("bottle", verse_num-1, false),
                one=one)
    } else {
        "No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.
".to_string()
    }

}


pub fn sing(from: u32, to: u32) -> String {
    let mut song : String = String::new();
    for verse_num in (to .. from+1).rev() {
        song += &verse(verse_num);
        if verse_num != to {
            song += "\n";
        }
    };
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
