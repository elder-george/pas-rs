use crate::parse::{expect, ident, number, sequence_with_sep};
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Simple {
        name: String,
    },
    Array {
        elem_type: Box<Type>,
        indices: Vec<Type>,
    },
    Range {
        start: i64,
        end: i64,
    },
}

impl Type {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        if let Ok(ts) = expect(&Token::Array, ts) {
            let ts = expect(&Token::LeftBr, ts)?;
            let (ts, indices) = sequence_with_sep(Type::parse, &Token::Comma, ts)?;
            if indices.len() == 0 {
                return Err("Array must have cardinality >= 0".to_string());
            }
            let ts = expect(&Token::RightBr, ts)?;
            let ts = expect(&Token::Of, ts)?;
            let (ts, elem_type) = Type::parse(ts)?;
            Ok((
                ts,
                Self::Array {
                    elem_type: Box::new(elem_type),
                    indices,
                },
            ))
        } else if let Ok((ts, start)) = number(ts) {
            let ts = expect(&Token::Ellipsis, ts)?;
            let (ts, end) = number(ts)?;
            Ok((ts, Self::Range { start, end }))
        } else if let Ok((ts, name)) = ident(ts) {
            Ok((ts, Self::Simple { name }))
        } else {
            Err(format!("expected type, got {:?}", ts[0]))
        }
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;
    use crate::token::tokenize_to_vec;

    #[test]
    fn simple_type() {
        assert_parses_into!(
            Type::parse(&tokenize_to_vec("integer")),
            Type::Simple {
                name: "integer".to_string()
            }
        );
    }

    #[test]
    fn range() {
        assert_parses_into!(
            Type::parse(&tokenize_to_vec("1..10")),
            Type::Range { start: 1, end: 10 }
        );
    }

    #[test]
    fn array() {
        assert_parses_into!(
            Type::parse(&tokenize_to_vec("array[1..10] of integer")),
            Type::Array {
                elem_type: Box::new(Type::Simple {
                    name: "integer".to_string()
                }),
                indices: vec![Type::Range { start: 1, end: 10 }],
            }
        );
        assert_parses_into!(
            Type::parse(&tokenize_to_vec("array[1..10, 1..10, char] of integer")),
            Type::Array {
                elem_type: Box::new(Type::Simple {
                    name: "integer".to_string()
                }),
                indices: vec![
                    Type::Range { start: 1, end: 10 },
                    Type::Range { start: 1, end: 10 },
                    Type::Simple {
                        name: "char".to_string()
                    }
                ],
            }
        );
    }
}
