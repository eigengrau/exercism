pub fn reply(s: &str) -> &str {
    if s.is_empty() {
        "Fine. Be that way!"
    } else if s.ends_with("?") {
        "Sure."
    } else if s.to_uppercase() == s {
        "Whoa, chill out!"
    } else {
        "Whatever."
    }
}
