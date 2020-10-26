#[cfg(test)]
macro_rules! assert_parses_into {
    ($actual:expr, $expected:expr) => {
        match $actual {
            Ok((ts_remainder, result)) => {
                assert_eq!(ts_remainder, &[Token::Eod] as &[Token]);
                assert_eq!(result, $expected);
            }
            e @ Err(..) => {
                panic!("assertion failed, expected Ok(..), got {:?}", e);
            }
        }
    };
}
