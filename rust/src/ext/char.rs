pub fn ascii_within(ranges: &'static [(u8, u8)]) -> impl Fn(&char) -> bool {
    return move |char| {
        if !char.is_ascii() {
            return false;
        }

        let char_ascii = *char as u8;
        for (start, end) in ranges {
            if *start <= char_ascii && char_ascii <= *end {
                return true;
            }
        }

        return false;
    };
}
