#[macro_export]
macro_rules! static_regex {
    ($name:tt, $regex:expr) => {
        fn $name() -> &'static regex::Regex {
            use std::sync::OnceLock;

            static REGEX: OnceLock<regex::Regex> = OnceLock::new();
            return REGEX
                .get_or_init(|| regex::Regex::new($regex).expect("regex could not be initialized"));
        }
    };
}
