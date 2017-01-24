pub fn hamming_distance(s1: &str, s2: &str) -> Result<usize, String> {

    if s1.len() != s2.len() {

        Err(String::from("hamming_distance: strings must be equally sized"))

    } else {

        let pairwise = s1.chars().zip(s2.chars());
        let distance = pairwise.filter(|&(c1, c2)| c1 != c2).count();
        Ok(distance)

    }

}
